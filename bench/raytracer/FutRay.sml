structure FutRay =
struct

  val profile = CommandLineArgs.parseFlag "profile"

  structure CtxSet = CtxSetFn (structure F = Futhark)

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
      ( if profile then (writeFile "futhark.json" (Futhark.Context.report ctx))
        else ()
      ; CtxSet.free ctxSet
      )
    end

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

  structure PreparedSceneSet =
  struct
    open CtxSet

    type scene_set = (device_identifier * prepared_scene) Seq.t

    fun prepareFromCtxSet (ctxSet: ctx_set) (height, width) =
      Seq.map
        (fn (device, ctx) =>
           let val scene = prepare_rgbbox_scene (ctx, height, width)
           in (device, scene)
           end) ctxSet

    fun freeScene (sceneSet: scene_set) =
      Seq.map (fn (_, scene) => prepare_rgbbox_scene_free scene) sceneSet

    fun choose (sceneSet: scene_set) (device: device_identifier) =
      let
        val (_, scene) = Seq.first
          (Seq.filter (fn (d, _) => d = device) sceneSet)
      in
        scene
      end
  end

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
