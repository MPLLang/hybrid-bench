structure FutMandelbrot =
struct

  type fut_context = FutharkMandelbrot.ctx

  fun init () =
      let val () = print "Initialising Futhark context... "
          val cfg = FutharkMandelbrot.Config.cache (SOME "futhark.cache") FutharkMandelbrot.Config.default
          val ctx = FutharkMandelbrot.Context.new cfg
          val () = print "Done!\n"
      in ctx end

  fun cleanup x = FutharkMandelbrot.Context.free x

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

end
