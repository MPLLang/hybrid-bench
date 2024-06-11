structure FutRay =
struct

  val profile = CommandLineArgs.parseFlag "profile"
  val devices = String.fields (fn c => c = #",")
    (CommandLineArgs.parseString "devices" "")
  val _ = print ("devices: " ^ String.concatWith "," devices ^ "\n")

  structure CtxSet = CtxSetFn (structure F = Futhark)

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
        let
          val _ =
            List.foldl
              (fn (ctx, idx) =>
                 let
                   val _ =
                     (writeFile ("futhark" ^ (Int.toString idx) ^ ".json")
                        (Futhark.Context.report ctx))
                 in
                   idx + 1
                 end) 0 (CtxSet.toCtxList ctxSet)
        in
          ()
        end


      else
        ()
    ; CtxSet.free ctxSet
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

  structure PreparedSceneSet =
  struct
    open CtxSet

    type scene_set = (device_identifier * prepared_scene) Seq.t

    fun prepareFromCtxSet (ctxSet: ctx_set) (height, width) =
      let
        val (device, ctx) = Seq.first ctxSet
        val scene = prepare_rgbbox_scene (ctx, height, width)
        val { prepared, ... } = scene
        val rawArray = Futhark.Opaque.prepared_scene.store prepared
      in
        Seq.map
          (fn (d, ctx) =>
             if d = device then
               (d, scene)
             else
               let
                 val prepared = Futhark.Opaque.prepared_scene.restore ctx (Word8ArraySlice.full rawArray)
                 val scene = { height = #height scene, width = #width scene, prepared = prepared}
               in
                 (d, scene)
               end) ctxSet
      end

    fun freeScene (sceneSet: scene_set) =
      Seq.map (fn (_, scene) => prepare_rgbbox_scene_free scene) sceneSet

    fun choose (sceneSet: scene_set) (device: device_identifier) =
      let
        val i =
          valOf (FindFirst.findFirstSerial (0, Seq.length sceneSet) (fn i =>
            #1 (Seq.nth sceneSet i) = device))
      in
        #2 (Seq.nth sceneSet i)
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
