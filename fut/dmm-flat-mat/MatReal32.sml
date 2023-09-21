structure MatReal32 =
struct

  type r32 = Real32.real
  type i64 = Int64.int

  type dmm_package = MLton.Pointer.t

  (* ====================== *)

  val rawSpawn =
    _import "dMMSpawn" public : r32 array * r32 array * r32 array * i64 -> dmm_package;

  val rawPoll = _import "dMMPoll" public : dmm_package -> Word8.word;

  val rawFinish = _import "dMMFinish" public : dmm_package -> unit;

  (* ====================== *)

  val rawFancySpawn =
    _import "fancy_dmm_spawn" public : MLton.Pointer.t * i64 * i64 * i64 * MLton.Pointer.t * i64 * i64 * i64 * r32 array * i64 * i64 * i64 * i64 -> dmm_package;

  val rawFancyPoll =
    _import "fancy_dmm_poll" public : dmm_package -> Word8.word;

  val rawFancyFinish = _import "fancy_dmm_finish" public : dmm_package -> unit;

  (* ====================== *)

  val rawFancyTwoSpawn =
    _import "fancy_two_dmm_spawn" public : MLton.Pointer.t * i64 * i64 * i64 * i64 * i64 * MLton.Pointer.t * i64 * i64 * i64 * i64 * i64 * r32 array * i64 * i64 * i64 * i64 -> dmm_package;

  val rawFancyTwoPoll =
    _import "fancy_two_dmm_poll" public : dmm_package -> Word8.word;

  val rawFancyTwoFinish =
    _import "fancy_two_dmm_finish" public : dmm_package -> unit;

  (* ====================== *)

  val memcpy_floats =
    _import "memcpy_floats" public : r32 array * i64 * r32 array * i64 * i64 -> unit;

  val cpu_sgemm =
    _import "cpu_sgemm" public : r32 array * r32 array * r32 array * i64 * i64 * i64 * bool -> unit;

  val memcpyFloatsToGpu =
    _import "memcpyFloatsToGpu" public : r32 array * i64 -> MLton.Pointer.t;

  val syncGpu = _import "synchronizeGpu" public : unit -> unit;

  val freeFloatsOnGpu =
    _import "freeFloatsOnGpu" public : MLton.Pointer.t -> unit;

  val leafSize = CommandLineArgs.parseInt "leaf-size" 256
  val gpuThresh = CommandLineArgs.parseInt "gpu-thresh" 512

  val _ = print ("leaf-size " ^ Int.toString leafSize ^ "\n")
  val _ = print ("gpu-thresh " ^ Int.toString gpuThresh ^ "\n")

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


  fun splitHorizontal (Slice {left, top, height, width, mat}) =
    let
      val h2 = height div 2

      val upper = Slice
        {left = left, top = top, height = h2, width = width, mat = mat}

      val lower = Slice
        { left = left
        , top = top + h2
        , height = height - h2
        , width = width
        , mat = mat
        }
    in
      (upper, lower)
    end


  fun splitVertical (Slice {left, top, height, width, mat}) =
    let
      val w2 = width div 2

      val first = Slice
        {left = left, top = top, height = height, width = w2, mat = mat}

      val second = Slice
        { left = left + w2
        , top = top
        , height = height
        , width = width - w2
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
                val start = rowskip slice * bigRow + left slice
              in
                Util.for (0, width slice) (fn i =>
                  Array.update (arr, start + i, f
                    { row = thisRow
                    , col = left slice + i - l
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

  (* ====================================================================== *)


  fun cpu_multiply_inplace (a, b, c) =
    if
      not
        (isSquare a andalso isSquare b andalso isSquare c
         andalso width a = width b andalso width a = width c)
    then
      raise MatrixFormat

    else if
      width a <= leafSize
    then
      let
        val n = width a
        val tmpA = makeCopy a
        val tmpB = makeCopy b
        val tmpC = makeCopy c
      in
        cpu_sgemm (data tmpA, data tmpB, data tmpC, n, n, n, true);
        copy {src = tmpC, dst = c}
      end

    else
      let
        val (a11, a12, a21, a22) = blocks a
        val (b11, b12, b21, b22) = blocks b
        val (c11, c12, c21, c22) = blocks c

        fun doBlock (m1, m2, m3, m4, c) =
          (cpu_multiply_inplace (m1, m2, c); cpu_multiply_inplace (m3, m4, c))
      in
        par4
          ( fn _ => doBlock (a11, b11, a12, b21, c11)
          , fn _ => doBlock (a11, b12, a12, b22, c12)
          , fn _ => doBlock (a21, b11, a22, b21, c21)
          , fn _ => doBlock (a21, b12, a22, b22, c22)
          );
        ()
      end


  and cpu_multiply (a, b) =
    if not (isSquare a andalso isSquare b andalso width a = width b) then
      raise MatrixFormat
    else
      let
        val n = width a
        val c = tabulate {width = n, height = n} (fn _ => 0.0)
      in
        cpu_multiply_inplace (a, b, c);
        c
      end


  (* ======================================================================
   * This is the more general algorithm, which allows for non-square sizes
   *
   * TODO: hybridize this algorithm.
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
        if maxdim <= leafSize then
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
            val (a1, a2) = splitHorizontal a
            val (c1, c2) = splitHorizontal c
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
            val (b1, b2) = splitVertical b
            val (c1, c2) = splitVertical c
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
            val (a1, a2) = splitVertical a
            val (b1, b2) = splitHorizontal b
          in
            cpu_multiply_nonsquare_inplace outputMode (a1, b1, c);
            cpu_multiply_nonsquare_inplace Accumulate (a2, b2, c)
          end
      end


  and cpu_multiply_nonsquare (a, b) =
    if not (width a = height b) then
      raise MatrixFormat
    else
      let val c = allocate {height = height a, width = width b}
      in cpu_multiply_nonsquare_inplace Write (a, b, c); c
      end


  (* ====================================================================== *)


  fun gpu_multiply (a, b) =
    if not (isSquare a andalso isSquare b andalso width a = width b) then
      raise MatrixFormat
    else
      let
        val n = width a
        val device_a = memcpyFloatsToGpu (data a, Array.length (data a))
        val device_b = memcpyFloatsToGpu (data b, Array.length (data b))
        val _ = syncGpu ()

        val result = ForkJoin.choice
          { cpu = fn () => Util.die "uh oh! gpu_multiply failed"
          , gpu =
              let
                fun spawn () =
                  let
                    val output = allocate {width = n, height = n}
                    val pkg = rawFancySpawn
                      ( (*data a*) device_a
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
                      )
                  in
                    (pkg, output)
                  end

                fun poll (pkg, _) =
                  (rawFancyPoll pkg = 0w1)

                fun finish (pkg, output) =
                  (rawFancyFinish pkg; output)
              in
                ForkJoin.gpu {spawn = spawn, poll = poll, finish = finish}
              end
          }
      in
        freeFloatsOnGpu device_a;
        freeFloatsOnGpu device_b;
        result
      end


  (* ====================================================================== *)


  fun hybrid_multiply_inplace (a, da) (b, db) c =
    if
      not
        (isSquare a andalso isSquare b andalso isSquare c
         andalso width a = width b andalso width a = width c)
    then
      raise MatrixFormat

    else if
      width a <= leafSize
    then
      let
        val n = width a
        val tmpA = makeCopy a
        val tmpB = makeCopy b
        val tmpC = makeCopy c
      in
        cpu_sgemm (data tmpA, data tmpB, data tmpC, n, n, n, true);
        copy {src = tmpC, dst = c}
      end

    else
      let
        val (a11, a12, a21, a22) = blocks a
        val (b11, b12, b21, b22) = blocks b
        val (c11, c12, c21, c22) = blocks c

        fun doBlock (m1, m2, m3, m4, c) =
          ( hybrid_multiply_inplace (m1, da) (m2, db) c
          ; hybrid_multiply_inplace_choose (m3, da) (m4, db) c
          )

        fun doBlockChoose (m1, m2, m3, m4, c) =
          hybrid_multiply_inplace_two_choose (m1, m3, da) (m2, m4, db) c
      in
        par4
          ( fn _ => doBlockChoose (a11, b11, a12, b21, c11)
          , fn _ => doBlockChoose (a11, b12, a12, b22, c12)
          , fn _ => doBlockChoose (a21, b11, a22, b21, c21)
          , fn _ => doBlock (a21, b12, a22, b22, c22)
          );
        ()
      end


  and hybrid_multiply_inplace_two_choose (a1, a2, da) (b1, b2, db) c =
    if width a1 < gpuThresh then
      ( hybrid_multiply_inplace (a1, da) (b1, db) c
      ; hybrid_multiply_inplace (a2, da) (b2, db) c
      )

    else
      let
        val n = width a1
      in
        ForkJoin.choice
          { cpu = fn () =>
              ( hybrid_multiply_inplace (a1, da) (b1, db) c
              ; hybrid_multiply_inplace_choose (a2, da) (b2, db) c
              )
          , gpu =
              let
                fun spawn () =
                  let
                    val tmp = ForkJoin.alloc (n * n)
                    val pkg = rawFancyTwoSpawn
                      ( (*data a*) da
                      , top a1
                      , left a1
                      , top a2
                      , left a2
                      , rowskip a1
                      , (*data b*) db
                      , top b1
                      , left b1
                      , top b2
                      , left b2
                      , rowskip b1

                      (* TODO: get rid of unused parameters, the 0s here *)
                      , (*data c*) tmp
                      , (*top c*) 0
                      , (*left c*) 0
                      , (*rowskip c*) 0
                      , n
                      )
                  in
                    (pkg, tmp)
                  end

                fun poll (pkg, _) =
                  (rawFancyTwoPoll pkg = 0w1)

                fun finish (pkg, tmp) =
                  (rawFancyTwoFinish pkg; tmp)

                fun cleanup tmp =
                  modify c (fn {row, col, v} =>
                    v + Array.sub (tmp, row * n + col))

              in
                ForkJoin.gpuWithCleanup
                  { spawn = spawn
                  , poll = poll
                  , finish = finish
                  , cleanup = cleanup
                  }
              end
          }
      end


  and hybrid_multiply_inplace_choose (a, da) (b, db) c =
    if width a < gpuThresh then
      hybrid_multiply_inplace (a, da) (b, db) c

    else
      let
        val n = width a
      in
        ForkJoin.choice
          { cpu = fn () => hybrid_multiply_inplace (a, da) (b, db) c
          , gpu =
              let
                fun spawn () =
                  let
                    val tmp = ForkJoin.alloc (n * n)
                    val pkg = rawFancySpawn
                      ( (*data a*) da
                      , top a
                      , left a
                      , rowskip a
                      , (*data b*) db
                      , top b
                      , left b
                      , rowskip b
                      , (*data c*) tmp
                      , (*top c*) 0
                      , (*left c*) 0
                      , (*rowskip c*) 0
                      , n
                      )
                  in
                    (pkg, tmp)
                  end

                fun poll (pkg, _) =
                  (rawFancyPoll pkg = 0w1)

                fun finish (pkg, tmp) =
                  (rawFancyFinish pkg; tmp)

                fun cleanup tmp =
                  modify c (fn {row, col, v} =>
                    v + Array.sub (tmp, row * n + col))
              in
                ForkJoin.gpuWithCleanup
                  { spawn = spawn
                  , poll = poll
                  , finish = finish
                  , cleanup = cleanup
                  }
              end
          }
      end


  and hybrid_multiply (a, b) =
    if not (isSquare a andalso isSquare b andalso width a = width b) then
      raise MatrixFormat
    else
      let
        val n = width a

        val (((da, db), c), tm) = Util.getTime (fn () =>
          ForkJoin.par
            ( fn () =>
                let
                  val da = memcpyFloatsToGpu (data a, Array.length (data a))
                  val db = memcpyFloatsToGpu (data b, Array.length (data b))
                in
                  syncGpu ();
                  (da, db)
                end

            , fn () =>
                let
                  val c = allocate {width = n, height = n}
                  val cData = data c
                  val _ = ForkJoin.parfor 5000 (0, n * n) (fn i =>
                    Array.update (cData, i, 0.0))
                in
                  c
                end
            ))

        val _ = print ("hybrid_multiply: setup time: " ^ Time.fmt 4 tm ^ "\n")


        val (_, tm) = Util.getTime (fn () =>
          hybrid_multiply_inplace (a, da) (b, db) c)

      in
        print ("hybrid_multiply_inplace time: " ^ Time.fmt 4 tm ^ "\n");
        freeFloatsOnGpu da;
        freeFloatsOnGpu db;
        c
      end

end
