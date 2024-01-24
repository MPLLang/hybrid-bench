structure CLA = CommandLineArgs

val height = CLA.parseInt "h" 200
val width = CLA.parseInt "w" 200
val output = CLA.parseString "output" ""
val dop6 = CLA.parseFlag "ppm6"
val impl = CLA.parseString "impl" "hybrid"
val scene_name = CLA.parseString "s" "rgbbox"
val scene =
  case scene_name of
    "rgbbox" => Ray.rgbbox
  | "irreg" => raise Fail ("irreg scene not implemented yet")
  | s => raise Fail ("No such scene: " ^ s)

val ctxSet = FutRay.init ()
val ctx = FutRay.CtxSet.getOne ctxSet

val _ = print ("h " ^ Int.toString height ^ "\n")
val _ = print ("w " ^ Int.toString width ^ "\n")
val _ = print ("output " ^ (if output = "" then "(none)" else output) ^ "\n")
val _ = print ("ppm6? " ^ (if dop6 then "yes" else "no") ^ "\n")
val _ = print ("s " ^ scene_name ^ "\n")

val bench =
  case impl of
    "cpu" =>
      (fn () =>
         let
           val ((objs, cam), tm1) = Util.getTime (fn _ =>
             Ray.from_scene width height scene)
           val _ = print ("Scene BVH construction in " ^ Time.fmt 4 tm1 ^ "s\n")
         in
           Ray.render_cpu objs width height cam
         end)

  | "gpu" =>
      (fn () =>
         let
           val (prepared_scene, tm2) = Util.getTime (fn _ =>
             FutRay.prepare_rgbbox_scene (ctx, height, width))
           val _ = print ("Futhark prep scene in " ^ Time.fmt 4 tm2 ^ "s\n")
           val result = Ray.render_gpu ctx prepared_scene width height
         in
           FutRay.prepare_rgbbox_scene_free prepared_scene;
           result
         end)

  | "hybrid" =>
      (fn () =>
         let
           val ((objs, cam), tm1) = Util.getTime (fn _ =>
             Ray.from_scene width height scene)
           val _ = print ("Scene BVH construction in " ^ Time.fmt 4 tm1 ^ "s\n")
           val (prepared_scene_set, tm2) = Util.getTime (fn _ =>
             FutRay.PreparedSceneSet.prepareFromCtxSet ctxSet (height, width))
           val _ = print ("Futhark prep scenes in " ^ Time.fmt 4 tm2 ^ "s\n")
           val result =
             Ray.render_hybrid ctxSet prepared_scene_set objs width height cam
         in
           FutRay.PreparedSceneSet.freeScene prepared_scene_set;
           result
         end)

  | _ => raise Fail ("unknown -impl: " ^ impl)

val result = Benchmark.run ("rendering (" ^ impl ^ ")") bench

val _ = FutRay.cleanup ctxSet

val writeImage = if dop6 then Ray.image2ppm6 else Ray.image2ppm

val _ =
  if output <> "" then
    let
      val out = TextIO.openOut output
    in
      print ("Writing image to " ^ output ^ ".\n");
      writeImage out (result);
      TextIO.closeOut out
    end
  else
    print ("-output not passed, so not writing image to file.\n")
