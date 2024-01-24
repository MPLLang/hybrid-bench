structure CLA = CommandLineArgs

val maxIter = (*CLA.parseInt "max-iter"*) 50
val divergeThresh = (*CLA.parseReal "diverge-thresh"*) 4.0

val _ = print ("max-iter " ^ Int.toString maxIter ^ "\n")
val _ = print ("diverge-thresh " ^ Real.toString divergeThresh ^ "\n")

(* pixels per unit of the complex plane *)
val resolution = (*CLA.parseInt "resolution"*) 5000
val _ = print ("resolution " ^ Int.toString resolution ^ "\n")

(* rectangular query region *)
val top = (*CLA.parseReal "top"*) 1.0
val bot = (*CLA.parseReal "bot"*) ~1.0
val left = (*CLA.parseReal "left"*) ~1.5
val right = (*CLA.parseReal "right"*) 0.5

val _ = print ("top " ^ Real.toString top ^ "\n")
val _ = print ("bot " ^ Real.toString bot ^ "\n")
val _ = print ("left " ^ Real.toString left ^ "\n")
val _ = print ("right " ^ Real.toString right ^ "\n")

(* derived width and height, in pixels *)
val w = Real.ceil (Real.fromInt resolution * (right - left))
val h = Real.ceil (Real.fromInt resolution * (top - bot))
val _ = print ("h " ^ Int.toString h ^ "\n")
val _ = print ("w " ^ Int.toString w ^ "\n")

val dx = (right - left) / Real.fromInt w
val dy = (top - bot) / Real.fromInt h

(* convert pixel coordinate to complex coordinate *)
fun xyToComplex (x, y) =
  let
    val r = left + (Real.fromInt x * dx)
    val i = bot + (Real.fromInt y * dy)
  in
    (r, i)
  end


fun mark x y =
  let
    val (cr, ci) = xyToComplex (x, y)

    fun loop (zr, zi, trmti) i =
      let
        val zi = 2.0 * zr * zi + ci
        val zr = trmti + cr
        val tr = zr * zr
        val ti = zi * zi
      in
        if tr + ti > divergeThresh then false
        else if i + 1 = maxIter then true
        else loop (zr, zi, tr - ti) (i + 1)
      end
  in
    loop (0.0, 0.0, 0.0) 0
  end


fun packByte y (xlo, xhi) =
  let
    fun loop byte x =
      if x >= xhi then
        byte
      else
        let
          val byte =
            if mark x y then Word8.orb (Word8.<< (byte, 0w1), 0w1)
            else Word8.<< (byte, 0w1)
        in
          loop byte (x + 1)
        end

    val byte = loop (0w0 : Word8.word) xlo

    val byte =
      if xhi - xlo = 8 then byte
      else Word8.<< (byte, Word.fromInt (8 - (xhi - xlo)))
  in
    byte
  end


(* ======================================================================== *)
(* ======================================================================== *)
(* ======================================================================== *)


val ctxSet = FutMandelbrot.init ()
val ctx = FutMandelbrot.CtxSet.getOne ctxSet


fun hybridMandelbrot () : Word8.word Seq.t Seq.t =
  let
    val numBytesPerRow = Util.ceilDiv w 8

    val rows = ForkJoin.alloc h
    fun putRow y row = Array.update (rows, y, row)

    fun do_cpu_row y =
      putRow y
        (ArraySlice.full (SeqBasis.tabulate 100 (0, numBytesPerRow) (fn b =>
           let
             val xlo = b * 8
             val xhi = Int.min (xlo + 8, w)
             val byte = packByte y (xlo, xhi)
           in
             byte
           end)))

    fun do_gpu_rows device (ylo, yhi) =
      let
        val outputArr = FutMandelbrot.mandelbrot (FutMandelbrot.CtxSet.choose ctxSet device) ylo yhi 0 numBytesPerRow
        fun slice i =
          ArraySlice.slice (outputArr, i * numBytesPerRow, SOME numBytesPerRow)
      in
        ForkJoin.parfor 1000 (0, yhi - ylo) (fn i => putRow (ylo + i) (slice i))
      end
  in
    HybridBasis.parfor_hybrid 0.5 10 (0, h) (do_cpu_row, do_gpu_rows);

    ArraySlice.full rows
  end


fun cpuMandelbrot () =
  let
    val numBytesPerRow = Util.ceilDiv w 8
  in
    ArraySlice.full (SeqBasis.tabulate 1 (0, h) (fn y =>
      ArraySlice.full (SeqBasis.tabulate 1000 (0, numBytesPerRow) (fn b =>
        let
          val xlo = b * 8
          val xhi = Int.min (xlo + 8, w)
          val byte = packByte y (xlo, xhi)
        in
          byte
        end))))
  end

fun gpuMandelbrot () =
  let
    val numBytesPerRow = Util.ceilDiv w 8
    val rows = ForkJoin.alloc h
    fun putRow y row = Array.update (rows, y, row)

    fun ensureOnGpu () =
      ForkJoin.choice
        { prefer_cpu = ensureOnGpu
        , prefer_gpu = fn _ =>
            let
              val outputArr = FutMandelbrot.mandelbrot ctx 0 h 0 numBytesPerRow
              fun slice i =
                ArraySlice.slice
                  (outputArr, i * numBytesPerRow, SOME numBytesPerRow)
            in
              ForkJoin.parfor 1000 (0, h) (fn i => putRow i (slice i))
            end
        }
  in
    ensureOnGpu ();
    ArraySlice.full rows
  end


(* ======================================================================== *)
(* ======================================================================== *)
(* ======================================================================== *)


val impl = CLA.parseString "impl" "hybrid"
val _ = print ("impl " ^ impl ^ "\n")

val bench =
  case impl of
    "cpu" => cpuMandelbrot
  | "gpu" => gpuMandelbrot
  | "hybrid" => hybridMandelbrot
  | _ => Util.die ("unknown impl: " ^ impl)


val results = Benchmark.run "running mandelbrot" bench

val _ = FutMandelbrot.cleanup ctxSet

val outfile = CLA.parseString "output" ""

fun app f s =
  Util.for (0, Seq.length s) (fn i => f (Seq.nth s i))

val _ =
  if outfile = "" then
    print ("use -output XXX to see result\n")
  else
    let
      val file = TextIO.openOut outfile
      fun dump1 c =
        TextIO.output1 (file, Char.chr (Word8.toInt c))
      fun dump str = TextIO.output (file, str)
    in
      ( dump "P4\n"
      ; dump (Int.toString w ^ " " ^ Int.toString h ^ "\n")
      ; app (app dump1) results
      ; TextIO.closeOut file
      ; print
          ("output written to " ^ outfile ^ "\n"
           ^ "(note: checkerboard pattern shows GPU work)\n")
      )
    end
