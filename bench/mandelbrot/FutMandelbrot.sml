structure FutMandelbrot =
struct

  val profile = CommandLineArgs.parseFlag "profile"

  structure CtxSet = CtxSetFn (structure F = FutharkMandelbrot)

  fun init () =
    let
      val () = print "Initialising Futhark context... "
      val ctxSet = CtxSet.fromList ["#0", "#1"]
      val () = print "Done!\n"
    in
      ctxSet
    end

  fun writeFile fname s =
    let val os = TextIO.openOut fname
    in TextIO.output (os, s) before TextIO.closeOut os
    end

  fun cleanup ctxSet =
  let 
  val (_, ctx) = Seq.first ctxSet (* FIXME *)
  in
    ( if profile then (writeFile "futhark.json" (FutharkMandelbrot.Context.report ctx))
      else ()
    ; CtxSet.free ctxSet
    )
    end

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
