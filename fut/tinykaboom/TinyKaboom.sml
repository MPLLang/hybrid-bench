structure TinyKaboom =
struct

  type f32 = f32.real

  val sphere_radius: f32 = 1.5
  val noise_amplitude: f32 = 1.0

  fun hash n =
    let val x = f32.sin (n) * 43758.5453
    in x - f32.realFloor (x)
    end

  type vec3 = vec3.vector

  fun vec3f (x, y, z) : vec3 = {x = x, y = y, z = z}

  fun lerp (v0, v1, t) =
    v0 + (v1 - v0) * f32.max 0.0 (f32.min 1.0 t)

  fun vlerp (v0, v1, t) =
    vec3.map2 (fn x => fn y => lerp (x, y, t)) v0 v1

  fun noise (x: vec3) =
    let
      val p =
        { x = f32.realFloor (#x x)
        , y = f32.realFloor (#y x)
        , z = f32.realFloor (#z x)
        }
      val f = {x = #x x - #x p, y = #y x - #y p, z = #z x - #z p}
      val f =
        vec3.scale
          (vec3.dot (f, vec3.sub
             ({x = 3.0, y = 3.0, z = 3.0}, vec3.scale 2.0 f))) f
      val n = vec3.dot (p, {x = 1.0, y = 57.0, z = 113.0})
    in
      lerp
        ( lerp
            ( lerp (hash (n + 0.0), hash (n + 1.0), #x f)
            , lerp (hash (n + 57.0), hash (n + 58.0), #x f)
            , #y f
            )
        , lerp
            ( lerp (hash (n + 113.0), hash (n + 114.0), #x f)
            , lerp (hash (n + 170.0), hash (n + 171.0), #x f)
            , #y f
            )
        , #z f
        )
    end

  fun rotate v =
    vec3f
      ( vec3.dot (vec3f (0.00, 0.80, 0.60), v)
      , vec3.dot (vec3f (~0.80, 0.36, ~0.48), v)
      , vec3.dot (vec3f (~0.60, ~0.48, 0.64), v)
      )

  fun fractal_brownian_motion (x: vec3) =
    let
      val p = rotate x
      val f = 0.0
      val f = f + 0.5000 * noise p
      val p = vec3.scale 2.32 p
      val f = f + 0.2500 * noise p
      val p = vec3.scale 3.03 p
      val f = f + 0.1250 * noise p
      val p = vec3.scale 2.61 p
      val f = f + 0.0625 * noise p
    in
      f / 0.9375
    end

  fun palette_fire (d: f32) : vec3 =
    let
      val yellow = vec3f (1.7, 1.3, 1.0)
      val orange = vec3f (1.0, 0.6, 0.0)
      val red = vec3f (1.0, 0.0, 0.0)
      val darkgray = vec3f (0.2, 0.2, 0.2)
      val gray = vec3f (0.4, 0.4, 0.4)

      val x = f32.max 0.0 (f32.min 1.0 d)
    in
      if x < 0.25 then vlerp (gray, darkgray, x * 4.0)
      else if x < 0.5 then vlerp (darkgray, red, x * 4.0 - 1.0)
      else if x < 0.75 then vlerp (red, orange, x * 4.0 - 2.0)
      else vlerp (orange, yellow, x * 4.0 - 3.0)
    end

  fun signed_distance t p =
    let
      val displacement =
        ~(fractal_brownian_motion (vec3.scale 3.4 p)) * noise_amplitude
    in
      vec3.norm p - (sphere_radius * f32.sin (t * 0.25) + displacement)
    end

  fun sq (x: f32) = x * x

  fun loop state continue f =
    if continue state then loop (f state) continue f else state

  fun sphere_trace t (orig: vec3, dir: vec3) : (bool * vec3) =
    let

      fun loop (i, pos) =
        if i >= 64 then
          (false, pos)
        else
          let
            val d = signed_distance t pos
          in
            if d < 0.0 then
              (true, pos)
            else
              loop (i + 1, vec3.add
                (pos, vec3.scale (f32.max (d * 0.1) 0.1) dir))
          end

    (* fun check (i, hit) = (i = 1337, hit) *)
    in
      if (vec3.dot (orig, orig)) - sq (vec3.dot (orig, dir)) > sq sphere_radius then
        (false, orig)
      else
        loop (0, orig)
    (* check (loop (0, orig) (fn (i, _) => i < 64) (fn (i, pos) =>
      let
        val d = signed_distance t pos
      in
        if d < 0.0 then (1337, pos)
        else (i + 1, vec3.add (pos, vec3.scale (f32.max (d * 0.1) 0.1) dir))
      end)) *)
    end

  fun distance_field_normal t pos =
    let
      val eps = 0.1
      val d = signed_distance t pos
      val nx = signed_distance t (vec3.add (pos, vec3f (eps, 0.0, 0.0))) - d
      val ny = signed_distance t (vec3.add (pos, vec3f (0.0, eps, 0.0))) - d
      val nz = signed_distance t (vec3.add (pos, vec3f (0.0, 0.0, eps))) - d
    in
      vec3.normalise (vec3f (nx, ny, nz))
    end

  fun rgb (r: f32) (g: f32) (b: f32) =
    let
      fun clamp x =
        f32.min 1.0 (f32.max 0.0 x)
      fun ch x =
        Word32.fromInt (f32.round (clamp x * 255.0))

      val x = ch r
      val x = Word32.orb (Word32.<< (x, 0w8), ch g)
      val x = Word32.orb (Word32.<< (x, 0w8), ch b)
    in
      x
    end


  fun pixelFunc t width height j i =
    let
      val fov = f32.pi / 3.0
      val dir_x = (f32.fromInt i + 0.5) - f32.fromInt width / 2.0
      val dir_y = ~(f32.fromInt j + 0.5) + f32.fromInt height / 2.0
      val dir_z = ~(f32.fromInt height) / (2.0 * f32.tan (fov / 2.0))
      val (is_hit, hit) = sphere_trace t (vec3f (0.0, 0.0, 3.0), vec3.normalise
        (vec3f (dir_x, dir_y, dir_z)))
    in
      if is_hit then
        let
          val noise_level = (sphere_radius - vec3.norm hit) / noise_amplitude
          val light_dir = vec3.normalise (vec3.sub
            (vec3f (10.0, 10.0, 10.0), hit))
          val light_intensity = f32.max 0.4 (vec3.dot
            (light_dir, distance_field_normal t hit))
          val {x, y, z} = vec3.scale light_intensity (palette_fire
            ((noise_level - 0.2) * 2.0))
        in
          rgb x y z
        end
      else
        rgb 0.2 0.7 0.8
    end


  fun compute_frame (t: f32) (width: int) (height: int) : Word32.word array =
    let
      fun f j i =
        pixelFunc t width height j i
    in
      SeqBasis.tabulate 10 (0, width * height) (fn k =>
        f (k div width) (k mod width))
    end


  val renderHybridGpuSplit =
    CommandLineArgs.parseReal "render-hybrid-gpu-split" 0.5
  val _ = print
    ("render-hybrid-gpu-split " ^ Real.toString renderHybridGpuSplit
     ^ " (fraction given to gpu choice points)\n")

  fun calculateMid lo hi =
    lo + Real.ceil (Real.fromInt (hi - lo) * (1.0 - renderHybridGpuSplit))


  fun compute_frame_hybrid ctx t width height =
    (* compute_frame t width height *)
    let
      val output = ForkJoin.alloc (width * height)

      fun writePixel k =
        Array.update
          (output, k, pixelFunc t width height (k div width) (k mod width))

      fun gpuTask lo hi =
        FutTinyKaboom.render_pixels ctx width height t (ArraySlice.slice
          (output, lo, SOME (hi - lo)))

      fun loop lo hi =
        if hi - lo <= 5000 then
          ForkJoin.parfor 100 (lo, hi) writePixel
        else
          let val mid = calculateMid lo hi
          in ForkJoin.par (fn _ => loop lo mid, fn _ => loopChoose mid hi); ()
          end

      and loopChoose lo hi =
        ForkJoin.choice {cpu = fn _ => loop lo hi, gpu = gpuTask lo hi}
    in
      loopChoose 0 (width * height);
      output
    end


  fun render_cpu frames fps width height =
    SeqBasis.tabulate 1 (0, frames) (fn frame =>
      { width = width
      , height = height
      , data = ArraySlice.full
          (compute_frame (f32.fromInt frame / f32.fromInt fps) width height)
      })


  fun render_gpu ctx frames fps width height =
    Array.tabulate (frames, fn frame =>
      { width = width
      , height = height
      , data =
          let
            val output = ArraySlice.full (ForkJoin.alloc (width * height))
          in
            ForkJoin.choice
              { cpu = fn _ => raise Fail "Uh oh! Should be impossible!"
              , gpu =
                  FutTinyKaboom.render_pixels ctx width height
                    (f32.fromInt frame / f32.fromInt fps) output
              };
            output
          end
      })


  fun render_hybrid ctx frames fps width height =
    SeqBasis.tabulate 1 (0, frames) (fn frame =>
      { width = width
      , height = height
      , data = ArraySlice.full
          (compute_frame_hybrid ctx (f32.fromInt frame / f32.fromInt fps) width
             height)
      })

end
