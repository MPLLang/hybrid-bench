(* MatCOO(I, R):
 *   - indices of type I.int
 *   - values of type R.real
 *
 * For example, the following defines matrices where the row and column indices
 * will be arrays of C type int32_t*, and values of C type float*
 *
 *   structure M = MatCOO(structure I = Int32
 *                        structure R = Real32)
 *
 * We can also use int64_t and doubles (or any other desired combination):
 *
 *   structure M = MatCOO(structure I = Int64
 *                        structure R = Real64)
 *)
functor MatCOO (structure I: INTEGER structure R: REAL) =
struct
  structure I = I
  structure R = R

  (* SoA format for storing every nonzero value = mat[row,column], i.e.:
   *   (row, column, value) = (row_indices[i], col_indices[i], values[i])
   *
   * so, number of nonzeros (nnz)
   * = length(row_indices)
   * = length(col_indices)
   * = length(values)
   *
   * we assume row_indices is sorted
   *
   * also, we keep track of lo/hi bounds on row and column indices:
   *   row_lo <= r < row_hi   for every r in row_indices
   *   col_lo <= c < col_hi   for every c in col_indices
   *)
  datatype mat =
    Mat of
      { row_lo: I.int
      , row_hi: I.int
      , col_lo: I.int
      , col_hi: I.int
      , row_indices: I.int Seq.t
      , col_indices: I.int Seq.t
      , values: R.real Seq.t
      }

  type t = mat

  fun nnz (Mat m) =
    Seq.length (#row_indices m)

  fun row_lo (Mat m) = #row_lo m
  fun row_hi (Mat m) = #row_hi m
  fun col_lo (Mat m) = #col_lo m
  fun col_hi (Mat m) = #col_hi m

  fun split_seq s k =
    (Seq.take s k, Seq.drop s k)

  (* split m -> (m1, m2)
   * where m = m1 + m2
   * and nnz(m1) ~= nnz(m2) 
   *)
  fun split (mat as Mat m) =
    let
      val half = nnz mat div 2

      val (r1, r2) = split_seq (#row_indices m) half
      val (c1, c2) = split_seq (#col_indices m) half
      val (v1, v2) = split_seq (#values m) half

      val row_hi_1 =
        if Seq.length r1 > 0 then I.+ (I.fromInt 1, Seq.last r1) else #row_lo m

      val row_lo_2 = if Seq.length r2 > 0 then Seq.first r2 else #row_hi m

      val m1 = Mat
        { row_lo = #row_lo m
        , row_hi = row_hi_1
        , col_lo = #col_lo m
        , col_hi = #col_hi m
        , row_indices = r1
        , col_indices = c1
        , values = v1
        }

      val m2 = Mat
        { row_lo = row_lo_2
        , row_hi = #row_hi m
        , col_lo = #col_lo m
        , col_hi = #col_hi m
        , row_indices = r2
        , col_indices = c2
        , values = v2
        }
    in
      (m1, m2)
    end


  fun dump_info msg (Mat m) =
    let
      val info = String.concatWith " "
        [ msg
        , I.toString (#row_lo m)
        , I.toString (#row_hi m)
        , I.toString (#col_lo m)
        , I.toString (#col_hi m)
        , Seq.toString I.toString (#row_indices m)
        , Seq.toString I.toString (#col_indices m)
        , Seq.toString (R.fmt (StringCvt.FIX (SOME 1))) (#values m)
        ]
    in
      print (info ^ "\n")
    end


  fun upd (a, i, x) =
    ( (*print ("upd " ^ Int.toString i ^ "\n");*)Array.update (a, i, x))


  (* =======================================================================
   * write_mxv: serial and parallel versions
   *
   * The first and last row of the input `mat` might overlap with other
   * parallel tasks, so these need to be returned separately and combined.
   *
   * All middle rows are "owned" by the call to write_mxv
   *)

  datatype write_mxv_result =
    SingleRowValue of R.real
  | FirstLastRowValue of R.real * R.real


  (* requires `row_lo mat < row_hi mat`, i.e., at least one row *)
  fun write_mxv_serial mat vec output : write_mxv_result =
    let
      val Mat {row_indices, col_indices, values, ...} = mat
      val n = nnz mat
    in
      if row_lo mat = I.- (row_hi mat, I.fromInt 1) then
        (* no writes to output; only a single result row value *)
        let
          (* val _ = dump_info "write_mxv_serial (single row)" mat *)
          val result = Util.loop (0, n) (R.fromInt 0) (fn (acc, i) =>
            R.+ (acc, R.*
              (Seq.nth vec (I.toInt (Seq.nth col_indices i)), Seq.nth values i)))
        in
          SingleRowValue result
        end
      else
        let
          (* val _ = dump_info "write_mxv_serial (multi rows)" mat *)
          fun single_row_loop r (i, acc) =
            if i >= n orelse Seq.nth row_indices i <> r then
              (i, acc)
            else
              let
                val acc' = R.+ (acc, R.*
                  ( Seq.nth vec (I.toInt (Seq.nth col_indices i))
                  , Seq.nth values i
                  ))
              in
                single_row_loop r (i + 1, acc')
              end

          val last_row = I.- (row_hi mat, I.fromInt 1)

          fun advance_to_next_nonempty_row i' r =
            let
              val next_r = Seq.nth row_indices i'
            in
              Util.for (1 + I.toInt r, I.toInt next_r) (fn rr =>
                upd (output, rr, R.fromInt 0));
              next_r
            end

          fun middle_loop r i =
            if r = last_row then
              i
            else
              let
                val (i', row_value) = single_row_loop r (i, R.fromInt 0)
              in
                upd (output, I.toInt r, row_value);
                middle_loop (advance_to_next_nonempty_row i' r) i'
              end

          val (i, first_row_value) =
            single_row_loop (row_lo mat) (0, R.fromInt 0)
          val i = middle_loop (advance_to_next_nonempty_row i (row_lo mat)) i
          val (_, last_row_value) = single_row_loop last_row (i, R.fromInt 0)
        in
          FirstLastRowValue (first_row_value, last_row_value)
        end
    end

  val nnzGrain = CommandLineArgs.parseInt "matcoo-nnz-grain" 5000

  (* requires `row_lo mat < row_hi mat`, i.e., at least one row *)
  fun write_mxv mat vec output : write_mxv_result =
    if nnz mat <= nnzGrain then
      write_mxv_serial mat vec output
    else
      let
        val (m1, m2) = split mat
        val (result1, result2) =
          ForkJoin.par (fn _ => write_mxv m1 vec output, fn _ =>
            write_mxv m2 vec output)

      (* val {first_row_value = frv1, last_row_value = lrv1} = result1
      val {first_row_value = frv2, last_row_value = lrv2} = result2 *)
      in
        (* dump_info "write_mxv" mat; *)

        if I.- (row_hi m1, I.fromInt 1) = row_lo m2 then
          case (result1, result2) of
            (SingleRowValue r1, SingleRowValue r2) =>
              SingleRowValue (R.+ (r1, r2))
          | (SingleRowValue r1, FirstLastRowValue (f2, l2)) =>
              FirstLastRowValue (R.+ (r1, f2), l2)
          | (FirstLastRowValue (f1, l1), SingleRowValue r2) =>
              FirstLastRowValue (f1, R.+ (l1, r2))
          | (FirstLastRowValue (f1, l1), FirstLastRowValue (f2, l2)) =>
              (* overlap *)
              ( (*print "fill in middle overlap\n"
                ;*) upd (output, I.toInt (row_lo m2), R.+ (l1, f2))
              ; FirstLastRowValue (f1, l2)
              )
        else
          let
            fun finish_l1 v =
              upd (output, I.toInt (row_hi m1) - 1, v)
            fun finish_f2 v =
              upd (output, I.toInt (row_lo m2), v)
            fun fill_middle () =
              ForkJoin.parfor 5000 (I.toInt (row_hi m1), I.toInt (row_lo m2))
                (fn r => upd (output, r, R.fromInt 0))
          in
            (* print "fill in middle, no overlap\n"; *)
            case (result1, result2) of
              (SingleRowValue r1, SingleRowValue r2) =>
                (fill_middle (); FirstLastRowValue (r1, r2))

            | (SingleRowValue r1, FirstLastRowValue (f2, l2)) =>
                (fill_middle (); finish_f2 f2; FirstLastRowValue (r1, l2))

            | (FirstLastRowValue (f1, l1), SingleRowValue r2) =>
                (finish_l1 l1; fill_middle (); FirstLastRowValue (f1, r2))

            | (FirstLastRowValue (f1, l1), FirstLastRowValue (f2, l2)) =>
                ( finish_l1 l1
                ; fill_middle ()
                ; finish_f2 f2
                ; FirstLastRowValue (f1, l2)
                )
          end
      end


  fun mxv (mat: mat) (vec: R.real Seq.t) =
    if nnz mat = 0 then
      Seq.tabulate (fn _ => R.fromInt 0) (Seq.length vec)
    else
      let
        val output: R.real array = ForkJoin.alloc (Seq.length vec)
        val result = write_mxv mat vec output
      in
        (* print "top level: fill in front\n"; *)
        ForkJoin.parfor 5000 (0, I.toInt (row_lo mat)) (fn r =>
          upd (output, r, R.fromInt 0));
        (* print "top-level: fill in middle\n"; *)
        case result of
          SingleRowValue r => upd (output, I.toInt (row_lo mat), r)
        | FirstLastRowValue (f, l) =>
            ( upd (output, I.toInt (row_lo mat), f)
            ; upd (output, I.toInt (row_hi mat) - 1, l)
            );
        (* print "top-level: fill in back\n"; *)
        ForkJoin.parfor 5000 (I.toInt (row_hi mat), Seq.length vec) (fn r =>
          upd (output, r, R.fromInt 0));
        ArraySlice.full output
      end


  fun fromFile path = raise Fail "not yet implemented"


  (* =================================================================== *)

  fun ii x = I.fromInt x

  val example1 = Mat
    { row_lo = ii 1
    , row_hi = ii 5
    , col_lo = ii 0
    , col_hi = ii 5
    , row_indices = Seq.map ii (Seq.fromList [1, 1, 1, 2, 4, 4])
    , col_indices = Seq.map ii (Seq.fromList [1, 3, 4, 2, 0, 4])
    , values = Seq.map (R.fromLarge IEEEReal.TO_NEAREST) (Seq.fromList
        [0.1, 0.1, 0.1, 0.1, 0.1, 0.1])
    }

end
