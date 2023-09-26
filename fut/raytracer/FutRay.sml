structure FutRay =
struct

  type fut_context = Ray.ctx

  fun init () =
      Ray.ctx_new Ray.default_cfg

  fun cleanup x =
      Ray.ctx_free x

  type i64 = Int64.int
  type i32 = Int32.int

  type prepared_scene = {prepared: Ray.Opaque.prepared_scene.t,
                         height: i64,
                         width: i64}

  fun prepare_rgbbox_scene (ctx, height, width) =
      let val scene = Ray.Entry.rgbbox ctx ()
      in {prepared=Ray.Entry.prepare_scene ctx (height, width, scene),
          height=height,
          width=width}
         before Ray.Opaque.scene.free scene
      end

  fun prepare_rgbbox_scene_free (scene : prepared_scene) =
      Ray.Opaque.prepared_scene.free (#prepared scene)

  fun render ctx {prepared,height,width} output : unit =
      let val (data, start, len) = Int32ArraySlice.base output
          val arr = Ray.Entry.render_pixels ctx (height, width, start, len, prepared)
          val () = Ray.Int32Array1.values_into arr output
          val () = Ray.Int32Array1.free arr
      in ()
      end

end
