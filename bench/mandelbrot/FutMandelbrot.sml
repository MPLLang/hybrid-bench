structure FutMandelbrot =
struct

  val profile = CommandLineArgs.parseFlag "profile"
  val devices = String.fields (fn c => c = #",")
    (CommandLineArgs.parseString "devices" "")

  structure CtxSet = CtxSetFn (structure F = FutharkMandelbrot)

  fun init () =
    let
      val () = print "Initialising Futhark context... "
      val ctxSet = CtxSet.fromList devices
      val () = print "Done!\n"
    in
      ctxSet
    end

  fun writeFile fname s =
    let val os = TextIO.openOut fname
    in TextIO.output (os, s) before TextIO.closeOut os
    end

  fun cleanup ctxSet =
    ( if profile then
        List.foldl
          ( fn (ctx, idx) =>
              (writeFile "futhark" ^ (Int.toString idx)
               ^ ".json" (FutharkMandelbrot.Context.report ctx))
          ; idx + 1
          ) 0 (CtxSet.toCtxList ctxSet)

      else
        ()
    ; CtxSet.free ctxSet
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
