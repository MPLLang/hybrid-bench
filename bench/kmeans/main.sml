val () = print ("K-means clustering.\n")

structure CLA = CommandLineArgs

val file = CLA.parseString "points" ""
val do_gen_random = CLA.parseFlag "gen-random-input"

val _ =
  if file <> "" andalso do_gen_random then
    Util.die ("--gen-random-input incompatible with -points ...")
  else
    ()

val d = CLA.parseInt "d" 2

val k = CLA.parseInt "k" 5

val impl = CommandLineArgs.parseString "impl" "cpu"

val () = if d = ~1 then raise Fail "Need -d INT" else ()

val points =
  if file = "" andalso not do_gen_random then
    raise Fail "Need either -points FILE or --gen-random-input -n NUM_POINTS"
  else if do_gen_random then
    let
      val num_points = CommandLineArgs.parseInt "n" 1000000
      val _ = print ("n " ^ Int.toString num_points ^ "\n")

      val resolution = 100000000
      fun gen i =
        Real.fromInt (Util.hash i mod resolution) / Real.fromInt resolution
      val data = Seq.tabulate gen (num_points * d)
    in
      Points.fromSeq d data
    end
  else
    let
      val () = print ("Reading points from " ^ file ^ "... ")
      val s = ParseFile.readSequenceReal file before print "Done!\n"
    in
      Points.fromSeq d s
      handle Size =>
        raise Fail
          ("Input file has " ^ Int.toString (Seq.length s)
           ^ " numbers, which cannot be interpreted as " ^ Int.toString d
           ^ "-dimensional points")
    end

val () = print ("Dims:   " ^ Int.toString d ^ "\n")
val () = print ("K:      " ^ Int.toString k ^ "\n")
val () = print ("Points: " ^ Int.toString (Points.length points) ^ "\n")

val max_iterations = 50

val () = print "Initialising Futhark context... "
val ctx = Futhark.Context.new
  (Futhark.Config.cache (SOME "futhark.cache") Futhark.Config.default)
val () = print "Done!\n"


fun futharkPoints (points: Points.t) =
  Futhark.Real64Array2.new ctx (Points.toSeq points) (Points.length points, d)

val points_fut = futharkPoints points

fun tt a b =
  Time.fmt 4 (Time.- (b, a))

val bench =
  case impl of
    "cpu-orig" => (fn () => Kmeans.kmeans k max_iterations points)
  | "cpu-alternate" => (fn () => Kmeans.kmeans' k max_iterations points)
  | "cpu" => (fn () => Kmeans.kmeans'' k max_iterations points)
  | "hybrid" =>
      (fn () =>
         let
           fun gpuHistogram centroids =
             let
               val centroids_fut =
                 Futhark.Real64Array2.new ctx (Points.toSeq centroids)
                   (Points.length centroids, d)
             in
               { kernel = fn (start, stop) =>
                   let
                     val t1 = Time.now ()
                     val hist_fut =
                       Futhark.Entry.histogram ctx
                         (points_fut, centroids_fut, start, stop - start)
                     val () = Futhark.Context.sync ctx

                     val t2 = Time.now ()
                     val hist_arr = Futhark.Real64Array2.values hist_fut
                     val () = Futhark.Real64Array2.free hist_fut
                     val t3 = Time.now ()
                     val result =
                       Seq.tabulate
                         (fn c =>
                            Seq.fromArraySeq (ArraySlice.slice
                              (hist_arr, c * (d + 1), SOME (d + 1)))) k
                     val t4 = Time.now ()
                   in
                     print
                       ("gpu histogram (" ^ Int.toString (stop - start) ^ "): "
                        ^ tt t1 t2 ^ "+" ^ tt t2 t3 ^ "+" ^ tt t3 t4 ^ "s\n");
                     result
                   end

               , after = fn () => Futhark.Real64Array2.free centroids_fut
               }
             end
         in
           Kmeans.kmeansNewHybrid gpuHistogram k max_iterations points
         end
         handle Futhark.Error msg => Util.die ("Futhark error: " ^ msg))

  | "gpu" =>
      (fn () =>
         let
           val t0 = Time.now ()
           val (num_iters, centroids_fut) =
             Futhark.Entry.kmeans ctx
               (Int64.fromInt k, Int32.fromInt max_iterations, points_fut)
           val t1 = Time.now ()
           val result = Points.fromSeq d (Seq.fromArraySeq
             (ArraySlice.full (Futhark.Real64Array2.values centroids_fut)))
           val () = Futhark.Real64Array2.free centroids_fut
           val t2 = Time.now ()
         in
           print ("gpu kmeans " ^ tt t0 t1 ^ "+" ^ tt t1 t2 ^ "s\n");
           (Int32.toInt num_iters, result)
         end)
  | "hybrid-orig" =>
      (fn () =>
         let
           fun centroidsChunkGPU (start, len, centroids) =
             let
               val centroids_fut =
                 Futhark.Real64Array2.new ctx (Points.toSeq centroids)
                   (Points.length centroids, d)
               val new_centroids_fut =
                 Futhark.Entry.centroids_chunk ctx
                   ( Int64.fromInt start
                   , Int64.fromInt len
                   , points_fut
                   , centroids_fut
                   )
             in
               Points.fromSeq d (Seq.fromArraySeq
                 (ArraySlice.full
                    (Futhark.Real64Array2.values new_centroids_fut)))
               before Futhark.Real64Array2.free centroids_fut
               before Futhark.Real64Array2.free new_centroids_fut
             end
           fun centroidsChunk arg =
             ForkJoin.choice
               { prefer_cpu = fn () => Kmeans.centroidsChunkCPU points arg
               , prefer_gpu = fn () => centroidsChunkGPU arg
               }
         in
           Kmeans.kmeansHybrid centroidsChunk k max_iterations points
         end)
  | _ => Util.die ("unknown -impl " ^ impl)

val (kmeans_iters, kmeans_res) = Benchmark.run ("kmeans " ^ impl) bench

val () = Futhark.Real64Array2.free points_fut
val () = Futhark.Context.free ctx

val () = print ("kmeans iterations: " ^ Int.toString kmeans_iters ^ "\n")
val _ = List.tabulate (k, fn i =>
  print
    (Util.summarizeArraySlice 10 Real.toString (Points.nth kmeans_res i) ^ "\n"))
