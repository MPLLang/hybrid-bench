(* Author: Troels Henriksen, https://sigkill.dk/
 *
 * Adapted by Sam Westrick
 *)

(* A ray tracer that fires one ray per pixel and only supports
coloured, reflective spheres.  It parallelises two things

 0. The construction of a BVH for accelerating ray lookups
    (divide-and-conquer task parallelism)

 1. The parallel loop across all of the pixels to be computed (data
    parallelism, albeit potentially poorly load balanced)

*)

structure Ray =
struct

  structure CtxSet = CtxSetFn (structure F = Futhark)

  structure Math = Real32.Math

  type vec3 = {x: Real32.real, y: Real32.real, z: Real32.real}

  local
    fun vf f (v1: vec3) (v2: vec3) =
      {x = f (#x v1, #x v2), y = f (#y v1, #y v2), z = f (#z v1, #z v2)}
  in

    val vec_add = vf (op+)
    val vec_sub = vf (op-)
    val vec_mul = vf (op*)
    val vec_div = vf (op/)

    fun scale s {x, y, z} =
      {x = s * x, y = s * y, z = s * z} : vec3

    fun dot (v1: vec3) (v2: vec3) =
      let val v3 = vec_mul v1 v2
      in #x v3 + #y v3 + #z v3
      end

    fun norm v =
      Math.sqrt (dot v v)

    fun normalise v =
      scale (1.0 / norm v) v

    fun cross {x = x1, y = y1, z = z1} {x = x2, y = y2, z = z2} =
      {x = y1 * z2 - z1 * y2, y = z1 * x2 - x1 * z2, z = x1 * y2 - y1 * x2} :
        vec3

  end

  type aabb = {min: vec3, max: vec3}

  fun min x y : Real32.real =
    if x < y then x else y

  fun max x y : Real32.real =
    if x < y then y else x

  fun enclosing (box0: aabb) (box1: aabb) =
    let
      val small =
        { x = min (#x (#min box0)) (#x (#min box1))
        , y = min (#y (#min box0)) (#y (#min box1))
        , z = min (#z (#min box0)) (#z (#min box1))
        }
      val big =
        { x = max (#x (#max box0)) (#x (#max box1))
        , y = max (#y (#max box0)) (#y (#max box1))
        , z = max (#z (#max box0)) (#z (#max box1))
        }
    in
      {min = small, max = big}
    end

  fun centre (aabb: aabb) =
    { x = (#x (#min aabb) + (#x (#max aabb) - #x (#min aabb)))
    , y = (#y (#min aabb) + (#y (#max aabb) - #y (#min aabb)))
    , z = (#z (#min aabb) + (#z (#max aabb) - #z (#min aabb)))
    }

  datatype 'a bvh = bvh_leaf of aabb * 'a | bvh_split of aabb * 'a bvh * 'a bvh

  fun bvh_aabb (bvh_leaf (box, _)) = box
    | bvh_aabb (bvh_split (box, _, _)) = box

  (* Couldn't find a sorting function in MLtons stdlib - this is from Rosetta Code. *)
  local
    fun merge cmp ([], ys) = ys
      | merge cmp (xs, []) = xs
      | merge cmp (xs as x :: xs', ys as y :: ys') =
          case cmp (x, y) of
            GREATER => y :: merge cmp (xs, ys')
          | _ => x :: merge cmp (xs', ys)
    fun sort cmp [] = []
      | sort cmp [x] = [x]
      | sort cmp xs =
          let
            val ys = List.take (xs, length xs div 2)
            val zs = List.drop (xs, length xs div 2)
          in
            merge cmp (sort cmp ys, sort cmp zs)
          end
  in
    fun mk_bvh f all_objs =
      let
        fun mk _ _ [] = raise Fail "mk_bvh: no nodes"
          | mk _ _ [x] =
              bvh_leaf (f x, x)
          | mk d n xs =
              let
                val axis =
                  case d mod 3 of
                    0 => #x
                  | 1 => #y
                  | _ => #z
                fun cmp (x, y) =
                  Real32.compare (axis (centre (f x)), axis (centre (f y)))
                val xs_sorted = sort cmp xs
                val xs_left = List.take (xs_sorted, n div 2)
                val xs_right = List.drop (xs_sorted, n div 2)
                fun do_left () =
                  mk (d + 1) (n div 2) xs_left
                fun do_right () =
                  mk (d + 1) (n - (n div 2)) xs_right
                val (left, right) =
                  if n < 100 then (do_left (), do_right ())
                  else ForkJoin.par (do_left, do_right)
                val box = enclosing (bvh_aabb left) (bvh_aabb right)
              in
                bvh_split (box, left, right)
              end
      in
        mk 0 (length all_objs) all_objs
      end
  end

  type pos = vec3
  type dir = vec3
  type colour = vec3

  val black: vec3 = {x = 0.0, y = 0.0, z = 0.0}
  val white: vec3 = {x = 1.0, y = 1.0, z = 1.0}

  type ray = {origin: pos, dir: dir}

  fun point_at_param (ray: ray) t =
    vec_add (#origin ray) (scale t (#dir ray))

  type hit = {t: Real32.real, p: pos, normal: dir, colour: colour}

  type sphere = {pos: pos, colour: colour, radius: Real32.real}

  fun sphere_aabb {pos, colour = _, radius} =
    { min = vec_sub pos {x = radius, y = radius, z = radius}
    , max = vec_add pos {x = radius, y = radius, z = radius}
    }

  fun sphere_hit {pos, colour, radius} r t_min t_max : hit option =
    let
      val oc = vec_sub (#origin r) pos
      val a = dot (#dir r) (#dir r)
      val b = dot oc (#dir r)
      val c = dot oc oc - radius * radius
      val discriminant = b * b - a * c
      fun try temp =
        if temp < t_max andalso temp > t_min then
          SOME
            { t = temp
            , p = point_at_param r temp
            , normal = scale (1.0 / radius)
                (vec_sub (point_at_param r temp) pos)
            , colour = colour
            }
        else
          NONE
    in
      if discriminant <= 0.0 then
        NONE
      else
        case try ((~b - Math.sqrt (b * b - a * c)) / a) of
          SOME hit => SOME hit
        | NONE => try ((~b + Math.sqrt (b * b - a * c)) / a)
    end

  fun aabb_hit aabb ({origin, dir}: ray) tmin0 tmax0 =
    let
      fun iter min' max' origin' dir' tmin' tmax' =
        let
          val invD = 1.0 / dir'
          val t0 = (min' - origin') * invD
          val t1 = (max' - origin') * invD
          val (t0', t1') = if invD < 0.0 then (t1, t0) else (t0, t1)
          val tmin'' = max t0' tmin'
          val tmax'' = min t1' tmax'
        in
          (tmin'', tmax'')
        end
      val (tmin1, tmax1) =
        iter (#x (#min aabb)) (#x (#max aabb)) (#x origin) (#x dir) tmin0 tmax0
    in
      if tmax1 <= tmin1 then
        false
      else
        let
          val (tmin2, tmax2) =
            iter (#y (#min aabb)) (#y (#max aabb)) (#y origin) (#y dir) tmin1
              tmax1
        in
          if tmax2 <= tmin2 then
            false
          else
            let
              val (tmin3, tmax3) =
                iter (#z (#min aabb)) (#z (#max aabb)) (#z origin) (#z dir)
                  tmin2 tmax2
            in
              not (tmax3 <= tmin3)
            end
        end
    end

  type objs = sphere bvh

  fun objs_hit (bvh_leaf (_, s)) r t_min t_max =
        sphere_hit s r t_min t_max
    | objs_hit (bvh_split (box, left, right)) r t_min t_max =
        if not (aabb_hit box r t_min t_max) then
          NONE
        else
          case objs_hit left r t_min t_max of
            SOME h =>
              (case objs_hit right r t_min (#t h) of
                 NONE => SOME h
               | SOME h' => SOME h')
          | NONE => objs_hit right r t_min t_max

  type camera = {origin: pos, llc: pos, horizontal: dir, vertical: dir}

  fun camera lookfrom lookat vup vfov aspect =
    let
      val theta = vfov * Math.pi / 180.0
      val half_height = Math.tan (theta / 2.0)
      val half_width = aspect * half_height
      val origin = lookfrom
      val w = normalise (vec_sub lookfrom lookat)
      val u = normalise (cross vup w)
      val v = cross w u
    in
      { origin = lookfrom
      , llc =
          vec_sub
            (vec_sub (vec_sub origin (scale half_width u)) (scale half_height v))
            w
      , horizontal = scale (2.0 * half_width) u
      , vertical = scale (2.0 * half_height) v
      }
    end

  fun get_ray (cam: camera) s t : ray =
    { origin = #origin cam
    , dir =
        vec_sub
          (vec_add (vec_add (#llc cam) (scale s (#horizontal cam)))
             (scale t (#vertical cam))) (#origin cam)
    }

  fun reflect v n =
    vec_sub v (scale (2.0 * dot v n) n)

  fun scatter (r: ray) (hit: hit) =
    let
      val reflected = reflect (normalise (#dir r)) (#normal hit)
      val scattered = {origin = #p hit, dir = reflected}
    in
      if dot (#dir scattered) (#normal hit) > 0.0 then
        SOME (scattered, #colour hit)
      else
        NONE
    end

  fun ray_colour objs r depth =
    case objs_hit objs r 0.001 1000000000.0 of
      SOME hit =>
        (case scatter r hit of
           SOME (scattered, attenuation) =>
             if depth < 50 then
               vec_mul attenuation (ray_colour objs scattered (depth + 1))
             else
               black
         | NONE => black)
    | NONE =>
        let
          val unit_dir = normalise (#dir r)
          val t = 0.5 * (#y unit_dir + 1.0)
          val bg = {x = 0.5, y = 0.7, z = 1.0}
        in
          vec_add (scale (1.0 - t) white) (scale t bg)
        end

  fun trace_ray objs width height cam j i : colour =
    let
      val u = Real32.fromInt i / Real32.fromInt width
      val v = Real32.fromInt j / Real32.fromInt height
      val ray = get_ray cam u v
    in
      ray_colour objs ray 0
    end

  type pixel = Int32.int

  fun colour_to_pixel
    {x = r: Real32.real, y = g: Real32.real, z = b: Real32.real} : pixel =
    let
      val s: Real32.real = 255.99
      val ir = Word32.fromInt (Real32.trunc (s * r))
      val ig = Word32.fromInt (Real32.trunc (s * g))
      val ib = Word32.fromInt (Real32.trunc (s * b))

      val x = Word32.orb
        (Word32.orb (Word32.<< (ir, 0w16), Word32.<< (ig, 0w8)), ib)
    in
      Int32.fromInt (Word32.toIntX x)
    end

  fun pixel_to_rgb (p: pixel) =
    let
      val p = Word32.fromInt (Int32.toInt p)
      val r = Word32.andb (0wxFF, Word32.>> (p, 0w16))
      val g = Word32.andb (0wxFF, Word32.>> (p, 0w8))
      val b = Word32.andb (0wxFF, p)
    in
      (Word32.toInt r, Word32.toInt g, Word32.toInt b)
    end

  type image = {pixels: pixel Array.array, height: int, width: int}

  fun image2ppm out ({pixels, height, width}: image) =
    let
      fun onPixel (r, g, b) =
        TextIO.output
          ( out
          , Int.toString r ^ " " ^ Int.toString g ^ " " ^ Int.toString b ^ "\n"
          )
    in
      TextIO.output
        ( out
        , "P3\n" ^ Int.toString width ^ " " ^ Int.toString height ^ "\n"
          ^ "255\n"
        ) before Array.app (onPixel o pixel_to_rgb) pixels
    end

  fun image2ppm6 out ({pixels, height, width}: image) =
    let
      fun onPixel (r, g, b) =
        TextIO.output (out, String.implode (List.map Char.chr [r, g, b]))
    in
      TextIO.output
        ( out
        , "P6\n" ^ Int.toString width ^ " " ^ Int.toString height ^ "\n"
          ^ "255\n"
        ) before Array.app (onPixel o pixel_to_rgb) pixels
    end


  val outer_split = BenchParams.Raytracer.outer_split
  val inner_split = BenchParams.Raytracer.inner_split
  val _ = print
    ("raytracer-outer-split " ^ Real.toString outer_split ^ "\n")
  val _ = print
    ("raytracer-inner-split " ^ Real.toString inner_split ^ "\n")

  val grain = BenchParams.Raytracer.grain

  fun render_hybrid ctxSet fut_prepared_scene_set objs width height cam : image =
    let
      val pixels: Int32.int array = ForkJoin.alloc (height * width)

      fun writePixel l =
        let
          val i = l mod width
          val j = height - l div width
        in
          Array.update (pixels, l, colour_to_pixel
            (trace_ray objs width height cam j i))
        end

      fun gpuTask device (lo, hi) =
        let
          val (tm1, tm2) =
            FutRay.render (CtxSet.choose ctxSet device)
              (FutRay.PreparedSceneSet.choose fut_prepared_scene_set device)
              (ArraySlice.slice (pixels, lo, SOME (hi - lo)))
        in
          print ("gpu " ^ Int.toString device ^ " (" ^ Int.toString (hi-lo) ^ "): " ^ Time.fmt 4 tm1 ^ "+" ^ Time.fmt 4 tm2 ^ "s\n")
        end

    in
      HybridBasis.parfor_hybrid outer_split inner_split grain
        (0, height * width) (writePixel, gpuTask);

      {width = width, height = height, pixels = pixels}
    end


  fun render_cpu objs width height cam : image =
    let
      val pixels: Int32.int array = ForkJoin.alloc (height * width)

      fun writePixel l =
        let
          val i = l mod width
          val j = height - l div width
        in
          Array.update (pixels, l, colour_to_pixel
            (trace_ray objs width height cam j i))
        end
    in
      ForkJoin.parfor 1000 (0, width * height) writePixel;
      {width = width, height = height, pixels = pixels}
    end


  fun render_gpu ctx fut_prepared_scene width height : image =
    let
      val pixels: Int32.int array = ForkJoin.alloc (height * width)
      val _ = FutRay.render ctx fut_prepared_scene (ArraySlice.full pixels)
    in
      {width = width, height = height, pixels = pixels}
    end


  type scene =
    { camLookFrom: pos
    , camLookAt: pos
    , camFov: Real32.real
    , spheres: sphere list
    }

  fun from_scene width height (scene: scene) : objs * camera =
    ( mk_bvh sphere_aabb (#spheres scene)
    , camera (#camLookFrom scene) (#camLookAt scene) {x = 0.0, y = 1.0, z = 0.0}
        (#camFov scene) (Real32.fromInt width / Real32.fromInt height)
    )

  fun tabulate_2d m n f =
    List.concat (List.tabulate (m, fn j => List.tabulate (n, fn i => f (j, i))))

  val rgbbox: scene =
    let
      val n = 10
      val k: Real32.real = 60.0

      val real = Real32.fromInt

      val leftwall = tabulate_2d n n (fn (y, z) =>
        { pos =
            { x = (~k / 2.0)
            , y = (~k / 2.0 + (k / real n) * real y)
            , z = (~k / 2.0 + (k / real n) * real z)
            }
        , colour = {x = 1.0, y = 0.0, z = 0.0}
        , radius = (k / (real n * 2.0))
        })

      val midwall = tabulate_2d n n (fn (x, y) =>
        { pos =
            { x = (~k / 2.0 + (k / real n) * real x)
            , y = (~k / 2.0 + (k / real n) * real y)
            , z = (~k / 2.0)
            }
        , colour = {x = 1.0, y = 1.0, z = 0.0}
        , radius = (k / (real n * 2.0))
        })

      val rightwall = tabulate_2d n n (fn (y, z) =>
        { pos =
            { x = (k / 2.0)
            , y = (~k / 2.0 + (k / real n) * real y)
            , z = (~k / 2.0 + (k / real n) * real z)
            }
        , colour = {x = 0.0, y = 0.0, z = 1.0}
        , radius = (k / (real n * 2.0))
        })


      val bottom = tabulate_2d n n (fn (x, z) =>
        { pos =
            { x = (~k / 2.0 + (k / real n) * real x)
            , y = (~k / 2.0)
            , z = (~k / 2.0 + (k / real n) * real z)
            }
        , colour = {x = 1.0, y = 1.0, z = 1.0}
        , radius = (k / (real n * 2.0))
        })


    in
      { spheres = leftwall @ midwall @ rightwall @ bottom
      , camLookFrom = {x = 0.0, y = 30.0, z = 30.0}
      , camLookAt = {x = 0.0, y = ~1.0, z = ~1.0}
      , camFov = 75.0
      }
    end

  val irreg: scene =
    let
      val n = 100
      val k: Real32.real = 600.0
      val real = Real32.fromInt
      val bottom = tabulate_2d n n (fn (x, z) =>
        { pos =
            { x = (~k / 2.0 + (k / real n) * real x)
            , y = 0.0
            , z = (~k / 2.0 + (k / real n) * real z)
            }
        , colour = white
        , radius = k / (real n * 2.0)
        })
    in
      { spheres = bottom
      , camLookFrom = {x = 0.0, y = 12.0, z = 30.0}
      , camLookAt = {x = 0.0, y = 10.0, z = ~1.0}
      , camFov = 75.0
      }
    end

end
