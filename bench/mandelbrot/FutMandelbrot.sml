structure FutMandelbrot =
struct

  val profile = CommandLineArgs.parseFlag "profile"

  type fut_context = FutharkMandelbrot.ctx

  fun init () =
    let
      val () = print "Initialising Futhark context... "
      val cfg =
        (FutharkMandelbrot.Config.cache (SOME "futhark.cache")
         o FutharkMandelbrot.Config.profiling profile)
          FutharkMandelbrot.Config.default
      val ctx = FutharkMandelbrot.Context.new cfg
      val () = print "Done!\n"
    in
      ctx
    end

  fun writeFile fname s =
    let val os = TextIO.openOut fname
    in TextIO.output (os, s) before TextIO.closeOut os
    end

  fun cleanup x =
    ( if profile then
        (writeFile "futhark.json" (FutharkMandelbrot.Context.report x))
      else
        ()
    ; FutharkMandelbrot.Context.free x
    )

  type i64 = Int64.int
  type i32 = Int32.int
  type u8 = Word8.word

  fun mandelbrot ctx ylo yhi blo bhi : u8 array =
    let
      val output = FutharkMandelbrot.Entry.mandelbrot ctx (ylo, yhi, blo, bhi)
      val _ = FutharkMandelbrot.Context.sync ctx
      val arr = FutharkMandelbrot.Word8Array1.values output
    in
      FutharkMandelbrot.Word8Array1.free output;
      arr
    end
    handle FutharkMandelbrot.Error msg => Util.die ("Futhark error: " ^ msg)

end
