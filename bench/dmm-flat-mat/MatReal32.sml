structure MatReal32 =
struct

  type r32 = Real32.real
  type i64 = Int64.int

  type dmm_package = MLton.Pointer.t

  (* ====================== *)

  val rawFancySpawn =
    _import "fancy_dmm_spawn" public : string * i64 * MLton.Pointer.t * i64 * i64 * i64 * MLton.Pointer.t * i64 * i64 * i64 * r32 array * i64 * i64 * i64 * i64 * i64 * i64 -> dmm_package;

  val rawFancyFinish = _import "fancy_dmm_finish" public : dmm_package -> unit;

  (* ====================== *)

  val memcpy_floats =
    _import "memcpy_floats" public : r32 array * i64 * r32 array * i64 * i64 -> unit;

  val cpu_sgemm =
    _import "cpu_sgemm" public : r32 array * r32 array * r32 array * i64 * i64 * i64 * bool -> unit;

  val memcpyFloatsToGpu =
    _import "memcpyFloatsToGpu" public : string * i64 * r32 array * i64 -> MLton.Pointer.t;

  val syncGpu = _import "synchronizeGpu" public : string * i64 -> unit;

  val freeFloatsOnGpu =
    _import "freeFloatsOnGpu" public : string * i64 * MLton.Pointer.t -> unit;

  (* ======================== *)

  fun memcpyFloatsGpuData devices (arr, len) : MLton.Pointer.t GpuData.t =
    ArraySlice.full (SeqBasis.tabulate 1 (0, Seq.length devices) (fn i =>
      let val gpu_id = Seq.nth devices i
      in (gpu_id, memcpyFloatsToGpu (gpu_id, String.size gpu_id, arr, len))
      end))

  fun freeFloatsGpuData (data: MLton.Pointer.t GpuData.t) =
    ForkJoin.parfor 1 (0, Seq.length data) (fn i =>
      let val (gpu_id, ptr) = Seq.nth data i
      in freeFloatsOnGpu (gpu_id, String.size gpu_id, ptr)
      end)

  fun syncGpus devices =
    ForkJoin.parfor 1 (0, Seq.length devices) (fn i =>
      let val gpu_id = Seq.nth devices i
      in syncGpu (gpu_id, String.size gpu_id)
      end);

  (* ======================== *)

  val leaf_size = BenchParams.DMM.leaf_size
  val gpu_thresh = BenchParams.DMM.gpu_thresh
  val split_frac = BenchParams.DMM.split_frac
  val _ = print ("dmm-leaf-size " ^ Int.toString leaf_size ^ "\n")
  val _ = print ("dmm-gpu-thresh " ^ Int.toString gpu_thresh ^ "\n")
  val _ = print ("dmm-split " ^ Real.toString split_frac ^ "\n")

  fun split n =
    Real.ceil (split_frac * Real.fromInt n)

  exception MatrixFormat

  (* ====================================================================== *)

  val par = ForkJoin.par

  fun par4 (a, b, c, d) =
    let val ((ar, br), (cr, dr)) = par (fn _ => par (a, b), fn _ => par (c, d))
    in (ar, br, cr, dr)
    end

  (* ====================================================================== *)

  datatype mat =
    Mat of {height: int, width: int, data: r32 array}

  datatype slice =
    Slice of {mat: mat, top: int, left: int, width: int, height: int}

  (* fun slice {left, top, height, width} mat =
    Slice {mat = mat, top = top, left = left, width = width, height = height} *)


  fun splitHorizontal (Slice {left, top, height, width, mat}) {half_height} =
    let
      val upper = Slice
        {left = left, top = top, height = half_height, width = width, mat = mat}

      val lower = Slice
        { left = left
        , top = top + half_height
        , height = height - half_height
        , width = width
        , mat = mat
        }
    in
      (upper, lower)
    end


  fun splitVertical (Slice {left, top, height, width, mat}) {half_width} =
    let
      val first = Slice
        {left = left, top = top, height = height, width = half_width, mat = mat}

      val second = Slice
        { left = left + half_width
        , top = top
        , height = height
        , width = width - half_width
        , mat = mat
        }
    in
      (first, second)
    end


  fun blocks (Slice {left, top, height, width, mat}) =
    let
      val w2 = width div 2
      val h2 = height div 2

      val b11 = Slice
        {left = left, top = top, height = h2, width = w2, mat = mat}

      val b12 = Slice
        { left = left + w2
        , top = top
        , height = h2
        , width = width - w2
        , mat = mat
        }
      val b21 = Slice
        { left = left
        , top = top + h2
        , height = height - h2
        , width = w2
        , mat = mat
        }
      val b22 = Slice
        { left = left + w2
        , top = top + h2
        , height = height - h2
        , width = width - w2
        , mat = mat
        }
    in
      (b11, b12, b21, b22)
    end


  fun width (Slice {width = w, ...}) = w
  fun height (Slice {height = h, ...}) = h
  fun top (Slice {top = t, ...}) = t
  fun left (Slice {left = l, ...}) = l
  fun mat (Slice {mat = m, ...}) = m
  fun data (Slice {mat = Mat {data = d, ...}, ...}) = d
  fun rowskip (Slice {mat = Mat {width = w, ...}, ...}) = w


  fun isSquare x =
    (width x = height x)


  fun copy {src, dst} =
    if width src * height src <= 100000 then
      let
        val Mat {width = dstBigWidth, data = dstData, ...} = mat dst
        val Mat {width = srcBigWidth, data = srcData, ...} = mat src

        fun blockRow j =
          let
            val dstStart = (j + top dst) * dstBigWidth + left dst
            val srcStart = (j + top src) * srcBigWidth + left src
          in
            memcpy_floats (dstData, dstStart, srcData, srcStart, width src)
          end
      in
        Util.for (0, height src) blockRow
      end
    else
      let
        val (s11, s12, s21, s22) = blocks src
        val (d11, d12, d21, d22) = blocks dst
      in
        par4
          ( fn _ => copy {src = s11, dst = d11}
          , fn _ => copy {src = s12, dst = d12}
          , fn _ => copy {src = s21, dst = d21}
          , fn _ => copy {src = s22, dst = d22}
          );
        ()
      end


  fun allocate {width, height} =
    Slice
      { left = 0
      , top = 0
      , width = width
      , height = height
      , mat = Mat
          { width = width
          , height = height
          , data = ForkJoin.alloc (height * width)
          }
      }


  fun makeCopy src =
    let val dst = allocate {width = width src, height = height src}
    in copy {src = src, dst = dst}; dst
    end


  fun modify s f =
    let
      val t = top s
      val l = left s

      fun doit slice =
        if width slice * height slice <= 10000 then
          let
            fun blockRow j =
              let
                val bigRow = j + top slice
                val thisRow = bigRow - t
                val arr = data slice
                val ll = left slice
                val start = rowskip slice * bigRow + ll
              in
                Util.for (0, width slice) (fn i =>
                  Array.update (arr, start + i, f
                    { row = thisRow
                    , col = ll + i - l
                    , v = Array.sub (arr, start + i)
                    }))
              end
          in
            Util.for (0, height slice) blockRow
          end
        else
          let
            val (b11, b12, b21, b22) = blocks slice
          in
            par4 (fn _ => doit b11, fn _ => doit b12, fn _ => doit b21, fn _ =>
              doit b22);
            ()
          end
    in
      doit s
    end


  fun tabulate {width = w, height = h} f =
    let
      val data = ForkJoin.alloc (w * h)

      fun fillIn slice =
        if width slice * height slice <= 10000 then
          let
            fun blockRow j =
              let
                val bigRow = j + top slice
                val start = w * bigRow + left slice
              in
                Util.for (0, width slice) (fn i =>
                  Array.update (data, start + i, f
                    {row = bigRow, col = left slice + i}))
              end
          in
            Util.for (0, height slice) blockRow
          end
        else
          let
            val (b11, b12, b21, b22) = blocks slice
          in
            par4
              ( fn _ => fillIn b11
              , fn _ => fillIn b12
              , fn _ => fillIn b21
              , fn _ => fillIn b22
              );
            ()
          end

      val whole = Slice
        { left = 0
        , top = 0
        , width = w
        , height = h
        , mat = Mat {width = w, height = h, data = data}
        }
    in
      fillIn whole;
      whole
    end


  (* ======================================================================
   * This is the more general algorithm, which allows for non-square sizes
   *)


  datatype output_mode =
    Accumulate
  | Write


  fun cpu_multiply_nonsquare_inplace outputMode (a, b, c) =
    if
      not
        ((width a = height b) andalso (height a = height c)
         andalso (width b = width c))
    then
      raise MatrixFormat
    else
      let
        val m = height a
        val n = width b
        val k = width a

        (* val _ = print (Int.toString m ^ " " ^ Int.toString n ^ " " ^ Int.toString ^ " " ^ k) *)

        val maxdim = Int.max (Int.max (m, n), k)
      in
        if maxdim <= leaf_size then
          case outputMode of
            Accumulate =>
              let
                val tmpA = makeCopy a
                val tmpB = makeCopy b
                val tmpC = makeCopy c
              in
                cpu_sgemm (data tmpA, data tmpB, data tmpC, m, n, k, true);
                copy {src = tmpC, dst = c}
              end

          | Write =>
              let
                val tmpA = makeCopy a
                val tmpB = makeCopy b
                val tmpC = allocate {height = m, width = n}
              in
                cpu_sgemm (data tmpA, data tmpB, data tmpC, m, n, k, false);
                copy {src = tmpC, dst = c}
              end


        else if maxdim = m then
          (* split a horizontally *)
          let
            val (a1, a2) = splitHorizontal a {half_height = height a div 2}
            val (c1, c2) = splitHorizontal c {half_height = height c div 2}
          (* val _ = print ("SPLIT HORIZONTAL   *)
          in
            par
              ( fn _ => cpu_multiply_nonsquare_inplace outputMode (a1, b, c1)
              , fn _ => cpu_multiply_nonsquare_inplace outputMode (a2, b, c2)
              );
            ()
          end


        else if maxdim = n then
          (* split b vertically *)
          let
            val (b1, b2) = splitVertical b {half_width = width b div 2}
            val (c1, c2) = splitVertical c {half_width = width c div 2}
          in
            par
              ( fn _ => cpu_multiply_nonsquare_inplace outputMode (a, b1, c1)
              , fn _ => cpu_multiply_nonsquare_inplace outputMode (a, b2, c2)
              );
            ()
          end


        else
          (* split a vertically and b horizontally *)
          let
            val (a1, a2) = splitVertical a {half_width = width a div 2}
            val (b1, b2) = splitHorizontal b {half_height = height b div 2}
          in
            cpu_multiply_nonsquare_inplace outputMode (a1, b1, c);
            cpu_multiply_nonsquare_inplace Accumulate (a2, b2, c)
          end
      end


  fun cpu_multiply_nonsquare (a, b) =
    if not (width a = height b) then
      raise MatrixFormat
    else
      let val c = allocate {height = height a, width = width b}
      in cpu_multiply_nonsquare_inplace Write (a, b, c); c
      end


  (* ====================================================================== *)


  fun gpu_multiply gpu_id (a, b) =
    if not (isSquare a andalso isSquare b andalso width a = width b) then
      raise MatrixFormat
    else
      let
        val n = width a
        val device_a = memcpyFloatsToGpu
          (gpu_id, String.size gpu_id, data a, Array.length (data a))
        val device_b = memcpyFloatsToGpu
          (gpu_id, String.size gpu_id, data b, Array.length (data b))
        val _ = syncGpu (gpu_id, String.size gpu_id)

        val output = allocate {width = n, height = n}
        val pkg = rawFancySpawn
          ( gpu_id
          , String.size gpu_id
          , (*data a*) device_a
          , top a
          , left a
          , rowskip a
          , (*data b*) device_b
          , top b
          , left b
          , rowskip b
          , (*data c*) data output
          , (*top c*) 0
          , (*left c*) 0
          , (*rowskip c)*) 0
          , n
          , n
          , n
          )
      in
        rawFancyFinish pkg;
        freeFloatsOnGpu (gpu_id, String.size gpu_id, device_a);
        freeFloatsOnGpu (gpu_id, String.size gpu_id, device_b);
        output
      end


  (* ====================================================================== *)

  fun check_dims (a, b, c) =
    if
      (width a = height b) andalso (height a = height c)
      andalso (width b = width c)
    then (height a, width b, width a)
    else raise MatrixFormat


  fun get_device_ptrs (da: MLton.Pointer.t GpuData.t, db: MLton.Pointer.t GpuData.t, dev_id) =
    let
      val (gpu_id, da) = Seq.nth da dev_id
      val (gpu_id', db) = Seq.nth db dev_id
      val _ =
        if gpu_id = gpu_id' then ()
        else raise Fail "whoops: get_device_ptrs gpu_id mixup"
    in
      (da, db, gpu_id)
    end


  fun hybrid_multiply_nonsquare_inplace_gpu dev_id (da, db) (a, b, c) =
    let
      val t0 = Time.now ()

      val (m, n, k) = check_dims (a, b, c)
      (* val _ = print ("calling cuBLAS...\n") *)
      val tmpC = allocate {height = m, width = n}
      val (da, db, gpu_id) = get_device_ptrs (da, db, dev_id)
      val pkg = rawFancySpawn
        ( gpu_id
        , String.size gpu_id
        , (*data a*) da
        , top a
        , left a
        , rowskip a
        , (*data b*) db
        , top b
        , left b
        , rowskip b
        , (*data c*) data tmpC
        , (*top c*) 0
        , (*left c*) 0
        , (*rowskip c*) 0
        , m
        , n
        , k
        )

      val _ = rawFancyFinish pkg
      val t1 = Time.now ()
    in
      print
        ("gpu " ^ Int.toString dev_id ^ ":" ^ gpu_id ^ " dmm(" ^ Int.toString m ^ "," ^ Int.toString n ^ ","
         ^ Int.toString k ^ ") " ^ Time.fmt 4 (Time.- (t1, t0)) ^ "s\n");

      tmpC
    end


  fun hybrid_multiply_nonsquare_inplace devices outputMode (da, db) (a, b, c) =
    let
      val (m, n, k) = check_dims (a, b, c)

      (* val _ = print
        ("hybrid_multiply_nonsquare_inplace " ^ Int.toString m ^ " "
         ^ Int.toString n ^ " " ^ Int.toString k ^ "\n") *)

      val maxdim = Int.max (Int.max (m, n), k)

    in
      if maxdim < gpu_thresh then
        cpu_multiply_nonsquare_inplace outputMode (a, b, c)

      else if maxdim = m then
        (* split a horizontally *)
        let
          val (a1, a2) = splitHorizontal a {half_height = split (height a)}
          val (c1, c2) = splitHorizontal c {half_height = split (height c)}
        (* val _ = print ("SPLIT HORIZONTAL   *)
        in
          par
            ( fn _ =>
                hybrid_multiply_nonsquare_inplace_choose devices outputMode (da, db)
                  (a1, b, c1)
            , fn _ =>
                hybrid_multiply_nonsquare_inplace devices outputMode (da, db)
                  (a2, b, c2)
            );
          ()
        end


      else if maxdim = n then
        (* split b vertically *)
        let
          val (b1, b2) = splitVertical b {half_width = split (width b)}
          val (c1, c2) = splitVertical c {half_width = split (width c)}
        in
          par
            ( fn _ =>
                hybrid_multiply_nonsquare_inplace_choose devices outputMode (da, db)
                  (a, b1, c1)
            , fn _ =>
                hybrid_multiply_nonsquare_inplace devices outputMode (da, db)
                  (a, b2, c2)
            );
          ()
        end


      else
        (* split a vertically and b horizontally *)
        let
          val (a1, a2) = splitVertical a {half_width = width a div 2}
          val (b1, b2) = splitHorizontal b {half_height = height b div 2}
        in
          hybrid_multiply_nonsquare_inplace devices outputMode (da, db) (a1, b1, c);
          hybrid_multiply_nonsquare_inplace devices Accumulate (da, db) (a2, b2, c)
        end
    end


  and hybrid_multiply_nonsquare_inplace_choose devices outputMode (da, db) (a, b, c) =
    let
      val (m, n, k) = check_dims (a, b, c)
      (* val _ = print
        ("hybrid_multiply_nonsquare_inplace_choose " ^ Int.toString m ^ " "
         ^ Int.toString n ^ " " ^ Int.toString k ^ "\n") *)
      val maxdim = Int.max (Int.max (m, n), k)
    in
      if maxdim < gpu_thresh then
        cpu_multiply_nonsquare_inplace outputMode (a, b, c)
        (* hybrid_multiply_nonsquare_inplace devices outputMode (da, db) (a, b, c) *)
      else
        let
          val choiceResult = ForkJoin.choice
            { prefer_cpu = fn _ =>
                ( hybrid_multiply_nonsquare_inplace devices outputMode (da, db)
                    (a, b, c)
                ; NONE
                )
            , prefer_gpu = fn dev =>
                SOME
                  (hybrid_multiply_nonsquare_inplace_gpu dev (da, db) (a, b, c))
            }
        in
          case choiceResult of
            NONE => ()
          | SOME tmpC =>
              case outputMode of
                Write => copy {src = tmpC, dst = c}
              | Accumulate =>
                  let
                    val arr = data tmpC
                  in
                    modify c (fn {row, col, v} =>
                      v + Array.sub (arr, row * n + col))
                  end
        end
    end


  fun hybrid_multiply_nonsquare (devices: Device.gpu_identifier Seq.t) (a, b) =
    if not (width a = height b) then
      raise MatrixFormat
    else
      let
        (* SAM_NOTE: no need to worry about this cost, this is essentially
         * free because it is uninitialized
         *)
        val c = allocate {height = height a, width = width b}

        val ((da, db), tm) = Util.getTime (fn () =>
          let
            val da = memcpyFloatsGpuData devices (data a, Array.length (data a))
            val db = memcpyFloatsGpuData devices (data b, Array.length (data b))
          in
            (* syncGpus devices; *)
            (da, db)
          end)
        val _ = print ("hybrid_multiply_nonsquare: setup time: " ^ Time.fmt 4 tm ^ "\n")

      in
        hybrid_multiply_nonsquare_inplace devices Write (da, db) (a, b, c);
        freeFloatsGpuData da;
        freeFloatsGpuData db;
        c
      end

end
