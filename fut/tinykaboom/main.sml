structure CLA = CommandLineArgs

val fps = CLA.parseInt "fps" 10
val width = CLA.parseInt "width" 640
val height = CLA.parseInt "height" 480
val frames = CLA.parseInt "frames" (10 * fps)
val outfile = CLA.parseString "outfile" ""
val impl = CLA.parseString "impl" "cpu"
(* val frame = CLA.parseInt "frame" 100 *)

val _ = print ("width " ^ Int.toString width ^ "\n")
val _ = print ("height " ^ Int.toString height ^ "\n")
(* val _ = print ("frame " ^ Int.toString frame ^ "\n") *)
val _ = print ("fps " ^ Int.toString fps ^ "\n")
val _ = print ("frames " ^ Int.toString frames ^ "\n")


val duration = Real.fromInt frames / Real.fromInt fps

val _ = print ("(" ^ Real.fmt (StringCvt.FIX (SOME 2)) duration ^ " seconds)\n")

val ctx = FutTinyKaboom.init ()

val render =
  case impl of
    "cpu" => (fn () => TinyKaboom.render_cpu frames fps width height)
  | "gpu" => (fn () => TinyKaboom.render_gpu ctx frames fps width height)
  | "hybrid" => (fn () => TinyKaboom.render_hybrid ctx frames fps width height)
  | _ => Util.die ("unknown impl: " ^ impl)

fun bench () =
  let
    val _ = print ("generating frames...\n")
    val (images, tm) = Util.getTime render
    val _ = print ("generated all frames in " ^ Time.fmt 4 tm ^ "s\n")
    val perFrame = Time.fromReal (Time.toReal tm / Real.fromInt frames)
    val _ = print ("average time per frame: " ^ Time.fmt 4 perFrame ^ "s\n")
  in
    images
  end

val images = Benchmark.run "tinykaboom" bench

val _ = FutTinyKaboom.cleanup ctx

val _ =
  if outfile = "" then
    print ("no output file specified; use -outfile XXX.gif to see result\n")
  else
    let
      val _ = print ("generating palette...\n")
      (* val palette = GIF.Palette.summarize [Color.white, Color.black] 256
        { width = width
        , height = height
        , data = ArraySlice.full (TinyKaboom.frame 5.1667 640 480)
        } *)

      fun unpackColor x =
        let
          val blue = Word32.andb (x, 0wxFF)
          val x = Word32.>> (x, 0w8)
          val green = Word32.andb (x, 0wxFF)
          val x = Word32.>> (x, 0w8)
          val red = Word32.andb (x, 0wxFF)
          val x = Word32.>> (x, 0w8)

          fun to8 w =
            Word8.fromLarge (Word32.toLarge w)
        in
          {red = to8 red, green = to8 green, blue = to8 blue}
        end

      fun sampleColor i =
        let
          val k = Util.hash i
          val frame = (k div (width * height)) mod frames
          val idx = k mod (width * height)
        in
          unpackColor (Seq.nth (#data (Array.sub (images, frame))) idx)
        end

      val palette =
        GIF.Palette.summarizeBySampling [Color.white, Color.black] 256
          sampleColor

      val blowUpFactor = CLA.parseInt "blowup" 1
      val _ = print ("blowup " ^ Int.toString blowUpFactor ^ "\n")

      fun blowUpImage (image as {width, height, data}) =
        if blowUpFactor = 1 then
          { width = width
          , height = height
          , data =
              ArraySlice.full
                (SeqBasis.tabulate 1000 (0, width * height) (fn k =>
                   unpackColor (Seq.nth data k)))
          }
        else
          let
            val width' = blowUpFactor * width
            val height' = blowUpFactor * height
            val output = ForkJoin.alloc (width' * height')
            val _ = ForkJoin.parfor 1 (0, height) (fn i =>
              ForkJoin.parfor (1000 div blowUpFactor) (0, width) (fn j =>
                let
                  val c = Seq.nth data (i * width + j)
                in
                  Util.for (0, blowUpFactor) (fn di =>
                    Util.for (0, blowUpFactor) (fn dj =>
                      Array.update
                        ( output
                        , (i * blowUpFactor + di) * width'
                          + (j * blowUpFactor + dj)
                        , unpackColor c
                        )))
                end))
          in
            {width = width', height = height', data = ArraySlice.full output}
          end

      val _ = print ("writing to " ^ outfile ^ "...\n")
      val msBetween = Real.round ((1.0 / Real.fromInt fps) * 100.0)
      val (_, tm) = Util.getTime (fn _ =>
        GIF.writeMany outfile msBetween palette
          { width = blowUpFactor * width
          , height = blowUpFactor * height
          , numImages = frames
          , getImage = fn i =>
              #remap palette (blowUpImage (Array.sub (images, i)))
          })
      val _ = print ("wrote all frames in " ^ Time.fmt 4 tm ^ "s\n")
    in
      ()
    end
