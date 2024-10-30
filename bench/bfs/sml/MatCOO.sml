(* NOTE: THIS IS MOSTLY A COPY/PASTE FROM ../sparse-mxv/MatCOO.sml
 * EXCEPT THAT I'VE ONLY KEPT THE MATRIX MARKET CODE...
 *)


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
functor MatCOO
  (structure I: INTEGER where type int = Int32.int
   structure R:
   sig
     include REAL
     structure W: WORD
     val castFromWord: W.word -> real
     val castToWord: real -> W.word
   end
   where type real = Real32.real) =
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
   *)
  datatype mat =
    Mat of
      { width: I.int
      , height: I.int
      , row_indices: I.int Seq.t
      , col_indices: I.int Seq.t
      , values: R.real Seq.t
      }

  type t = mat

  fun width (Mat m) = #width m
  fun height (Mat m) = #height m

  fun nnz (Mat m) =
    Seq.length (#row_indices m)

  fun row_lo (mat as Mat m) =
    if nnz mat = 0 then I.fromInt 0 else Seq.first (#row_indices m)

  fun row_hi (mat as Mat m) =
    if nnz mat = 0 then I.fromInt 0
    else I.+ (I.fromInt 1, Seq.last (#row_indices m))

  fun row_spread mat =
    I.toInt (row_hi mat) - I.toInt (row_lo mat)

  (* ======================================================================= *)
  (* ======================================================================= *)
  (* =======================================================================
   * to/from file
   *)

  structure DS = DelayedSeq

  fun fromMatrixMarketFile path chars =
    let
      val lines = ParseFile.tokens (fn c => c = #"\n") chars
      val numLines = DS.length lines
      fun line i : char DS.t = DS.nth lines i
      fun lineStr i : string =
        let val ln = line i
        in CharVector.tabulate (DS.length ln, DS.nth ln)
        end

      fun lineIsComment i =
        let val ln = line i
        in DS.length ln > 0 andalso DS.nth ln 0 = #"%"
        end

      val _ =
        if
          numLines > 0
          andalso
          ParseFile.eqStr "%%MatrixMarket matrix coordinate real general"
            (line 0)
        then ()
        else raise Fail ("MatCOO.fromFile: not sure how to parse file: " ^ path)

      val firstNonCommentLineNum =
        case FindFirst.findFirst 1000 (1, numLines) (not o lineIsComment) of
          SOME i => i
        | NONE => raise Fail ("MatCOO.fromFile: missing contents?")

      fun fail () =
        raise Fail
          ("MatCOO.fromFile: error parsing line "
           ^ Int.toString (1 + firstNonCommentLineNum)
           ^ ": expected <num-rows> <num-cols> <num-values>")
      val (numRows, numCols, numValues) =
        let
          val lineNum = firstNonCommentLineNum
          val lnChars = DS.toArraySeq (line lineNum)
          val toks = ParseFile.tokens Char.isSpace lnChars
          val nr = valOf (ParseFile.parseInt (DS.nth toks 0))
          val nc = valOf (ParseFile.parseInt (DS.nth toks 1))
          val nv = valOf (ParseFile.parseInt (DS.nth toks 2))
        in
          (nr, nc, nv)
        end
        handle _ => fail ()

      val _ = print ("num rows    " ^ Int.toString numRows ^ "\n")
      val _ = print ("num cols    " ^ Int.toString numCols ^ "\n")
      val _ = print ("num nonzero " ^ Int.toString numValues ^ "\n")
      val _ = print ("parsing elements (may take a while...)\n")

      val row_indices = ForkJoin.alloc numValues
      val col_indices = ForkJoin.alloc numValues
      val values = ForkJoin.alloc numValues

      fun fail lineNum =
        raise Fail
          ("MatCOO.fromFile: error parsing line " ^ Int.toString (1 + lineNum)
           ^ ": expected <row> <col> <value>")

      (* TODO: this is very slow *)
      fun parseValue i =
        let
          val lineNum = firstNonCommentLineNum + 1 + i
          val lnChars = DS.toArraySeq (line lineNum)
          val toks = ParseFile.tokens Char.isSpace lnChars
          val r = I.fromInt (valOf (ParseFile.parseInt (DS.nth toks 0)))
          val c = I.fromInt (valOf (ParseFile.parseInt (DS.nth toks 1)))
          val v = R.fromLarge IEEEReal.TO_NEAREST (valOf (ParseFile.parseReal
            (DS.nth toks 2)))

        (* val ln = line lineNum
        val chars = CharVector.tabulate (DS.length ln, DS.nth ln)
        val toks = String.tokens Char.isSpace chars
        val r = I.fromInt (valOf (Int.fromString (List.nth (toks, 0))))
        val c = I.fromInt (valOf (Int.fromString (List.nth (toks, 1))))
        val v = R.fromLarge IEEEReal.TO_NEAREST (valOf (Real.fromString
          (List.nth (toks, 2)))) *)
        in
          (* if i mod 500000 = 0 then
            print ("finished row " ^ Int.toString i ^ "\n")
          else
            (); *)

          (* coordinates are stored in .mtx files using 1-indexing, but we
           * want 0-indexing
           *)
          Array.update (row_indices, i, I.- (r, I.fromInt 1));
          Array.update (col_indices, i, I.- (c, I.fromInt 1));
          Array.update (values, i, v)
        end
        handle _ => fail (firstNonCommentLineNum + 1 + i)

      val _ = ForkJoin.parfor 1000 (0, numValues) parseValue
      val _ = print ("finished parsing elements\n")
      val _ = print ("formatting...\n")

      val getSorted =
        let
          val idx = Seq.tabulate (fn i => i) numValues
        in
          StableSort.sortInPlace
            (fn (i, j) =>
               I.compare
                 (Array.sub (row_indices, i), Array.sub (row_indices, j))) idx;
          fn i => Seq.nth idx i
        end

      val row_indices =
        Seq.tabulate (fn i => Array.sub (row_indices, getSorted i)) numValues
      val col_indices =
        Seq.tabulate (fn i => Array.sub (col_indices, getSorted i)) numValues
      val values =
        Seq.tabulate (fn i => Array.sub (values, getSorted i)) numValues

      val _ = print ("done parsing " ^ path ^ "\n")
    in
      Mat
        { width = I.fromInt numCols
        , height = I.fromInt numRows
        , row_indices = row_indices
        , col_indices = col_indices
        , values = values
        }
    end


  (* MatrixCoordinateRealBin\n
   * [8 bits unsigned: real value precision, either 32 or 64]
   * [64 bits unsigned: number of rows]
   * [64 bits unsigned: number of columns]
   * [64 bits unsigned: number of elements]
   * [element]
   * [element]
   * ...
   *
   * each element is as follows, where X is the real value precision (32 or 64)
   * [64 bits unsigned: row index][64 bits unsigned: col index][X bits: value]
   *)
  fun writeBinFile mat path =
    let
      val file = TextIO.openOut path
      val _ = TextIO.output (file, "MatrixCoordinateRealBin\n")
      val _ = TextIO.closeOut file

      val file = BinIO.openAppend path

      fun w8 (w: Word8.word) = BinIO.output1 (file, w)

      fun w32 (w: Word64.word) =
        let
          open Word64
          infix 2 >> andb
        in
          w8 (Word8.fromLarge (w >> 0w24));
          w8 (Word8.fromLarge (w >> 0w16));
          w8 (Word8.fromLarge (w >> 0w8));
          w8 (Word8.fromLarge w)
        end

      fun w64 (w: Word64.word) =
        let
          open Word64
          infix 2 >> andb
        in
          (* this will only work if Word64 = LargeWord, which is good. *)
          w8 (Word8.fromLarge (w >> 0w56));
          w8 (Word8.fromLarge (w >> 0w48));
          w8 (Word8.fromLarge (w >> 0w40));
          w8 (Word8.fromLarge (w >> 0w32));
          w8 (Word8.fromLarge (w >> 0w24));
          w8 (Word8.fromLarge (w >> 0w16));
          w8 (Word8.fromLarge (w >> 0w8));
          w8 (Word8.fromLarge w)
        end

      fun wr64 (r: R.real) =
        w64 (R.W.toLarge (R.castToWord r))
      fun wr32 (r: R.real) =
        w32 (R.W.toLarge (R.castToWord r))

      val (wr, rsize) =
        case R.W.wordSize of
          32 => (wr32, 0w32)
        | 64 => (wr64, 0w64)
        | _ =>
            raise Fail
              "MatCOO.writeBinFile: only 32-bit and 64-bit reals supported"
    in
      w8 rsize;
      w64 (Word64.fromInt (I.toInt (height mat)));
      w64 (Word64.fromInt (I.toInt (width mat)));
      w64 (Word64.fromInt (nnz mat));
      Util.for (0, nnz mat) (fn i =>
        let
          val Mat {row_indices, col_indices, values, ...} = mat
          val r = Seq.nth row_indices i
          val c = Seq.nth col_indices i
          val v = Seq.nth values i
        in
          w64 (Word64.fromInt (I.toInt r));
          w64 (Word64.fromInt (I.toInt c));
          wr v
        end);
      BinIO.closeOut file
    end


  fun fromBinFile path bytes =
    let
      val header = "MatrixCoordinateRealBin\n"
      val header' =
        if Seq.length bytes < String.size header then
          raise Fail ("MatCOO.fromBinFile: missing or incomplete header")
        else
          CharVector.tabulate (String.size header, fn i =>
            Char.chr (Word8.toInt (Seq.nth bytes i)))
      val _ =
        if header = header' then
          ()
        else
          raise Fail
            ("MatCOO.fromBinFile: expected MatrixCoordinateRealBin header")

      val bytes = Seq.drop bytes (String.size header)

      fun r64 off =
        let
          infix 2 << orb
          val op<< = Word64.<<
          val op orb = Word64.orb

          val w = Word8.toLarge (Seq.nth bytes off)
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off + 1)))
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off + 2)))
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off + 3)))
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off + 4)))
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off + 5)))
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off + 6)))
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off + 7)))
        in
          w
        end

      fun r32 off =
        let
          infix 2 << orb
          val op<< = Word64.<<
          val op orb = Word64.orb

          val w = Word8.toLarge (Seq.nth bytes off)
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off + 1)))
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off + 2)))
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off + 3)))
        in
          w
        end

      fun r8 off = Seq.nth bytes off
      fun rr32 off =
        R.castFromWord (R.W.fromLarge (r32 off))
      fun rr64 off =
        R.castFromWord (R.W.fromLarge (r64 off))

      (* ====================================================================
       * parse binary contents
       *)

      val rsize = Word8.toInt (r8 0)

      fun rsizeFail () =
        raise Fail
          ("MatCOO.fromBinFile: found " ^ Int.toString rsize
           ^ "-bit reals, but expected " ^ Int.toString R.W.wordSize ^ "-bit")

      val (rr, rbytes) =
        if rsize = R.W.wordSize then
          case rsize of
            32 => (rr32, 4)
          | 64 => (rr64, 8)
          | _ => rsizeFail ()
        else
          rsizeFail ()

      val elemSize = 8 + 8 + rbytes
      val elemStartOff = 1 + 8 + 8 + 8

      val height = I.fromInt (Word64.toInt (r64 (1 + 0)))
      val width = I.fromInt (Word64.toInt (r64 (1 + 8)))
      val numValues = Word64.toInt (r64 (1 + 8 + 8))

      val row_indices = ForkJoin.alloc numValues
      val col_indices = ForkJoin.alloc numValues
      val values = ForkJoin.alloc numValues
    in
      ForkJoin.parfor 5000 (0, numValues) (fn i =>
        let
          val off = elemStartOff + i * elemSize
          val r = I.fromInt (Word64.toInt (r64 off))
          val c = I.fromInt (Word64.toInt (r64 (off + 8)))
          val v = rr (off + 8 + 8)
        in
          Array.update (row_indices, i, r);
          Array.update (col_indices, i, c);
          Array.update (values, i, v)
        end);

      Mat
        { width = width
        , height = height
        , row_indices = ArraySlice.full row_indices
        , col_indices = ArraySlice.full col_indices
        , values = ArraySlice.full values
        }
    end


  fun fromFile path =
    let
      val file = TextIO.openIn path
      val _ = print ("loading " ^ path ^ "\n")

      val h1 = "%%MatrixMarket"
      val h2 = "MatrixCoordinateRealBin\n"

      val actualHeader = TextIO.inputN
        (file, Int.max (String.size h1, String.size h2))
    in
      TextIO.closeIn file;

      if String.isPrefix h1 actualHeader then
        fromMatrixMarketFile path (ReadFile.contentsSeq path)
      else if String.isPrefix h2 actualHeader then
        fromBinFile path (ReadFile.contentsBinSeq path)
      else
        raise Fail ("unknown header " ^ actualHeader)
    end

end
