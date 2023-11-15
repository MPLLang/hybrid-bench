structure FutRay =
struct

  val profile = CommandLineArgs.parseFlag "profile"

  type fut_context = Futhark.ctx

  fun init () =
    let
      val () = print "Initialising Futhark context... "
      val cfg =
        (Futhark.Config.cache (SOME "futhark.cache")
         o Futhark.Config.profiling profile) Futhark.Config.default
      val ctx = Futhark.Context.new cfg
      val () = print "Done!\n"
    in
      ctx
    end


  fun writeFile fname s =
    let val os = TextIO.openOut fname
    in TextIO.output (os, s) before TextIO.closeOut os
    end

  fun cleanup x =
    ( if profile then (writeFile "futhark.json" (Futhark.Context.report x))
      else ()
    ; Futhark.Context.free x
    )

  type i64 = Int64.int
  type i32 = Int32.int

  type prepared_scene =
    {prepared: Futhark.Opaque.prepared_scene.t, height: i64, width: i64}

  fun prepare_rgbbox_scene (ctx, height, width) =
    let
      val scene = Futhark.Entry.rgbbox ctx ()
    in
      { prepared = Futhark.Entry.prepare_scene ctx (height, width, scene)
      , height = height
      , width = width
      } before Futhark.Opaque.scene.free scene
    end
    handle Futhark.Error msg => Util.die ("Futhark error: " ^ msg)

  fun prepare_rgbbox_scene_free (scene: prepared_scene) =
    Futhark.Opaque.prepared_scene.free (#prepared scene)

  fun render ctx {prepared, height, width} output : unit =
    let
      val (_, start, len) = ArraySlice.base output
      val arr = Futhark.Entry.render_pixels ctx
        (height, width, start, len, prepared)
      val () = Futhark.Int32Array1.values_into arr output
      val () = Futhark.Int32Array1.free arr
    in
      ()
    end
    handle Futhark.Error msg => Util.die ("Futhark error: " ^ msg)

end
