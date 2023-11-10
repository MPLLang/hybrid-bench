structure FutRay =
struct

  val profile = CommandLineArgs.parseFlag "profile"

  type fut_context = Ray.ctx

  fun init () =
    let
      val () = print "Initialising Futhark context... "
      val cfg =
        (Ray.Config.cache (SOME "futhark.cache") o Ray.Config.profiling profile)
          Ray.Config.default
      val ctx = Ray.Context.new cfg
      val () = print "Done!\n"
    in
      ctx
    end


  fun writeFile fname s =
    let val os = TextIO.openOut fname
    in TextIO.output (os, s) before TextIO.closeOut os
    end

  fun cleanup x =
    ( if profile then (writeFile "futhark.json" (Ray.Context.report x)) else ()
    ; Ray.Context.free x
    )

  type i64 = Int64.int
  type i32 = Int32.int

  type prepared_scene =
    {prepared: Ray.Opaque.prepared_scene.t, height: i64, width: i64}

  fun prepare_rgbbox_scene (ctx, height, width) =
    let
      val scene = Ray.Entry.rgbbox ctx ()
    in
      { prepared = Ray.Entry.prepare_scene ctx (height, width, scene)
      , height = height
      , width = width
      } before Ray.Opaque.scene.free scene
    end
    handle Ray.Error msg => Util.die ("Futhark error: " ^ msg)

  fun prepare_rgbbox_scene_free (scene: prepared_scene) =
    Ray.Opaque.prepared_scene.free (#prepared scene)

  fun render ctx {prepared, height, width} output : unit =
    let
      val (data, start, len) = ArraySlice.base output
      val arr = Ray.Entry.render_pixels ctx
        (height, width, start, len, prepared)
      val () = Ray.Int32Array1.values_into arr output
      val () = Ray.Int32Array1.free arr
    in
      ()
    end
    handle Ray.Error msg => Util.die ("Futhark error: " ^ msg)

end
