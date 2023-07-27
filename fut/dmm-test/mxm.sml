type dmm_package = MLton.Pointer.t

val rawdMMSpawn =
  _import "dMMSpawn" public : Real32.real array * Real32.real array * Real32.real array * Int64.int -> dmm_package;

val rawdMMPoll = _import "dMMPoll" public : dmm_package -> Word8.word;

val rawdMMFinish = _import "dMMFinish" public : dmm_package -> unit;


val cpu_sgemm =
  _import "cpu_sgemm" public : Real32.real array * Real32.real array * Real32.real array * Int64.int -> unit;

val memcpy_floats =
  _import "memcpy_floats" public : Real32.real array * Int64.int * Real32.real array * Int64.int * Int64.int -> unit;

val leafSize = CommandLineArgs.parseInt "leaf-size" 256

structure HybridTreeMatrix =
struct

  val par = ForkJoin.par

  fun par4 (a, b, c, d) =
    let val ((ar, br), (cr, dr)) = par (fn _ => par (a, b), fn _ => par (c, d))
    in (ar, br, cr, dr)
    end

  datatype matrix =
    Node of int * matrix * matrix * matrix * matrix
  | Leaf of int * Real32.real Array.array

  exception MatrixFormat

  fun sidelength mat =
    case mat of
      Leaf (n, s) => n
    | Node (n, _, _, _, _) => n

  fun tabulate sidelen f =
    let
      fun tab n (row, col) =
        if n <= leafSize then
          let
            val data = ForkJoin.alloc (n * n)
            fun blockRow i =
              let
                val start = i * n
              in
                Util.for (0, n) (fn j =>
                  Array.update (data, start + j, f (row + i, col + j)))
              end
          in
            Util.for (0, n) blockRow;
            Leaf (n, data)
          end

        else
          let
            val half = n div 2
            val (m11, m12, m21, m22) =
              par4
                ( fn _ => tab half (row, col)
                , fn _ => tab half (row, col + half)
                , fn _ => tab half (row + half, col)
                , fn _ => tab half (row + half, col + half)
                )
          in
            Node (n, m11, m12, m21, m22)
          end
    in
      tab sidelen (0, 0)
    end


  fun modify mat f =
    let
      fun doit (row, col) mat =
        case mat of
          Leaf (n, data) =>
            let
              fun blockRow i =
                let
                  val start = i * n
                in
                  Util.for (0, n) (fn j =>
                    Array.update (data, start + j, f
                      (row + i, col + j, Array.sub (data, start + j))))
                end
            in
              Util.for (0, n) blockRow
            end

        | Node (n, m11, m12, m21, m22) =>
            let
              val half = n div 2
            in
              par4
                ( fn _ => doit (row, col) m11
                , fn _ => doit (row, col + half) m12
                , fn _ => doit (row + half, col) m21
                , fn _ => doit (row + half, col + half) m22
                );
              ()
            end
    in
      doit (0, 0) mat
    end


  val upd = Array.update


  fun writeFlatten (result, rowskip, row, col) m =
    case m of
      Leaf (n, s) =>
        let
          fun blockRow i =
            memcpy_floats (result, (row + i) * rowskip + col, s, 0, n)
        in
          ForkJoin.parfor (Int.max (1, 5000 div leafSize)) (0, n) blockRow
        end

    | Node (n, m11, m12, m21, m22) =>
        let
          val half = n div 2
        in
          par4
            ( fn _ => writeFlatten (result, rowskip, row, col) m11
            , fn _ => writeFlatten (result, rowskip, row, col + half) m12
            , fn _ => writeFlatten (result, rowskip, row + half, col) m21
            , fn _ => writeFlatten (result, rowskip, row + half, col + half) m22
            );
          ()
        end


  fun flatten m =
    let
      val n = sidelength m
      val result = ForkJoin.alloc (n * n)
    in
      writeFlatten (result, n, 0, 0) m;
      result
    end


  fun sequentialWriteFlatten (result, rowskip, row, col) m =
    case m of
      Leaf (n, s) =>
        let
          fun blockRow i =
            memcpy_floats (result, (row + i) * rowskip + col, s, 0, n)
        in
          Util.for (0, n) blockRow
        end

    | Node (n, m11, m12, m21, m22) =>
        let
          val half = n div 2
        in
          sequentialWriteFlatten (result, rowskip, row, col) m11;
          sequentialWriteFlatten (result, rowskip, row, col + half) m12;
          sequentialWriteFlatten (result, rowskip, row + half, col) m21;
          sequentialWriteFlatten (result, rowskip, row + half, col + half) m22
        end


  fun sequentialFlatten m =
    let
      val n = sidelength m
      val result = ForkJoin.alloc (n * n)
    in
      sequentialWriteFlatten (result, n, 0, 0) m;
      result
    end


  fun flatmultiply n (s, t, output) = cpu_sgemm (s, t, output, n)
  (*let
    val sub = Array.sub
    val a = s
    val b = t
    val aStart = 0
    val bStart = 0
    (* assume our lengths are good *)
    (* loop with accumulator to compute dot product. r is an index into
     * vector a (the row index) and c is an index into b (the col index) *)
    fun loop rowStop acc r c =
      if r = rowStop then
        acc
      else
        let
          val acc' = acc + (sub (a, r) * sub (b, c))
          val r' = r + 1
          val c' = c + n
        in
          loop rowStop acc' r' c'
        end
    fun cell c =
      let
        val (i, j) = (c div n, c mod n)
        val rowStart = aStart + i * n
        val rowStop = rowStart + n
        val colStart = bStart + j
      in
        loop rowStop 0.0 rowStart colStart
      end
    fun update i =
      let
        val newv = cell i
        val old = sub (output, i)
      in
        Array.update (output, i, newv + old)
      end
    fun loopi i hi =
      if i >= hi then () else (update i; loopi (i + 1) hi)
  in
    loopi 0 (n * n)
  end
  *)


  fun makedMMOnGpuTaskLeaf input1 input2 output n =
    let
      fun spawn () = rawdMMSpawn (input1, input2, output, n)

      fun poll pack =
        (0w1 = rawdMMPoll pack)

      fun finish pack = rawdMMFinish pack
    in
      ForkJoin.gpu {spawn = spawn, poll = poll, finish = finish}
    end


  fun makedMMOnGpuTaskLeafWithCleanup input1 input2 output n cleanup =
    let
      fun spawn () = rawdMMSpawn (input1, input2, output, n)

      fun poll pack =
        (0w1 = rawdMMPoll pack)

      fun finish pack = rawdMMFinish pack
    in
      ForkJoin.gpuWithCleanup
        {spawn = spawn, poll = poll, finish = finish, cleanup = cleanup}
    end


  fun dMMHybridBenchmarkLeaf a b output n =
    ForkJoin.choice
      { gpu = makedMMOnGpuTaskLeaf a b output n
      , cpu = (fn () => flatmultiply n (a, b, output))
      }


  fun hybrid_multiply' (a, b, c) =
    case (a, b, c) of
    (* (Leaf (n, s), Leaf (_, t), Leaf (_, c)) => flatmultiply n (s, t, c) *)
      (Leaf (n, s), Leaf (_, t), Leaf (_, c)) => dMMHybridBenchmarkLeaf s t c n
    | ( Node (n, a11, a12, a21, a22)
      , Node (_, b11, b12, b21, b22)
      , Node (_, c11, c12, c21, c22)
      ) =>
        let
          fun block (m1, m2, m3, m4, c) =
            (hybrid_multiply' (m1, m2, c); hybrid_multiply' (m3, m4, c))

          (* TODO: more efficient call to GPU possible here. Bundle up both
           * calls as a single GPU task.
           *)
          fun blockChoose (m1, m2, m3, m4, c) =
            ( hybrid_multiplyChoose (m1, m2, c)
            ; hybrid_multiplyChoose (m3, m4, c)
            )
        in
          par4
            ( fn _ => block (a11, b11, a12, b21, c11)
            , fn _ => block (a11, b12, a12, b22, c12)
            , fn _ => blockChoose (a21, b11, a22, b21, c21)
            , fn _ => blockChoose (a21, b12, a22, b22, c22)
            );
          ()
        end
    | _ => raise MatrixFormat


  and hybrid_multiplyChoose (a, b, c) =
    case (a, b, c) of
      (Leaf (n, s), Leaf (_, t), Leaf (_, c)) => dMMHybridBenchmarkLeaf s t c n
    | (Node _, Node _, Node _) =>
        ForkJoin.choice
          { cpu = fn () => hybrid_multiply' (a, b, c)
          , gpu =
              let
                fun spawn () =
                  let
                    val n = sidelength a
                    val aFlat = sequentialFlatten a
                    val bFlat = sequentialFlatten b
                    val cFlat = sequentialFlatten c
                    val pkg = rawdMMSpawn (aFlat, bFlat, cFlat, n)
                  in
                    (pkg, cFlat, n)
                  end

                fun poll (pkg, _, _) =
                  (0w1 = rawdMMPoll pkg)

                fun finish (pkg, cFlat, n) =
                  (rawdMMFinish pkg; (cFlat, n))

                fun cleanup (cFlat, n) =
                  modify c (fn (row, col, _) =>
                    Array.sub (cFlat, row * n + col))
              in
                ForkJoin.gpuWithCleanup
                  { spawn = spawn
                  , poll = poll
                  , finish = finish
                  , cleanup = cleanup
                  }
              end
          }


  fun multiply (a, b) =
    let val c = tabulate (sidelength a) (fn _ => 0.0)
    in hybrid_multiply' (a, b, c); c
    end


  fun cpu_multiply' (a, b, c) =
    case (a, b, c) of
      (Leaf (n, s), Leaf (_, t), Leaf (_, c)) => flatmultiply n (s, t, c)
    | ( Node (n, a11, a12, a21, a22)
      , Node (_, b11, b12, b21, b22)
      , Node (_, c11, c12, c21, c22)
      ) =>
        let
          fun block (m1, m2, m3, m4, c) =
            (cpu_multiply' (m1, m2, c); cpu_multiply' (m3, m4, c))
        in
          par4
            ( fn _ => block (a11, b11, a12, b21, c11)
            , fn _ => block (a11, b12, a12, b22, c12)
            , fn _ => block (a21, b11, a22, b21, c21)
            , fn _ => block (a21, b12, a22, b22, c22)
            );
          ()
        end
    | _ => raise MatrixFormat


  fun cpu_multiply (a, b) =
    let val c = tabulate (sidelength a) (fn _ => 0.0)
    in cpu_multiply' (a, b, c); c
    end


  fun gpu_multiply (a, b) =
    let
      val n = sidelength a
      val aFlat = sequentialFlatten a
      val bFlat = sequentialFlatten b
      val output = Array.tabulate (n * n, fn _ => 0.0)
    in
      dMMHybridBenchmarkLeaf aFlat bFlat output n;
      tabulate n (fn (row, col) => Array.sub (output, row * n + col))
    end

end
