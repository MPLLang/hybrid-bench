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

val profile = CommandLineArgs.parseFlag "profile"

val devices = String.fields (fn c => c = #",")
  (CommandLineArgs.parseString "devices" "")

val max_iterations = CLA.parseInt "max-iterations" 10

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
val () = print ("Max iterations: " ^ Int.toString max_iterations ^ "\n")

structure CtxSet = CtxSetFn (structure F = Futhark)
val () = print "Initialising Futhark context... "
val ctxSet = CtxSet.fromList devices
val ctx = CtxSet.getOne ctxSet
val () = print "Done!\n"

structure FutharkPoints = GpuData(type t = Futhark.Real64Array2.array)

fun futharkPoints (points: Points.t) ctx =
  Futhark.Real64Array2.new ctx (Points.toSeq points) (Points.length points, d)

val points_fut_set = FutharkPoints.initialize ctxSet (futharkPoints points)
val points_fut = FutharkPoints.choose points_fut_set "#1"

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
               val centroids_fut_set =
                 FutharkPoints.initialize ctxSet (fn ctx =>
                   Futhark.Real64Array2.new ctx (Points.toSeq centroids)
                     (Points.length centroids, d))
             in
               { kernel = fn device =>
                   let
                     val ctx = CtxSet.choose ctxSet device
                     val centroids_fut =
                       FutharkPoints.choose centroids_fut_set device
                     val points_fut = FutharkPoints.choose points_fut_set device
                   in
                     fn (start, stop) =>
                       let
                         val t1 = Time.now ()
                         val (counts_fut, hist_fut) =
                           Futhark.Entry.histogram ctx
                             (points_fut, centroids_fut, start, stop - start)
                         val () = Futhark.Context.sync ctx

                         val t2 = Time.now ()
                         val counts_arr = Futhark.Int64Array1.values counts_fut
                         val hist_arr = Futhark.Real64Array2.values hist_fut
                         val () = Futhark.Int64Array1.free counts_fut
                         val () = Futhark.Real64Array2.free hist_fut
                         val t3 = Time.now ()
                         val result =
                           ArraySlice.full (Array.tabulate (k, fn c =>
                             Array.tabulate (d + 1, fn i =>
                               if i = 0 then
                                 Real.fromInt (Array.sub (counts_arr, c))
                               else
                                 Array.sub (hist_arr, c * d + (i - 1)))))
                         val t4 = Time.now ()
                       in
                         (* print
                           ("gpu kmeans (" ^ Int.toString (stop - start) ^ "): "
                            ^ tt t1 t2 ^ "+" ^ tt t2 t3 ^ "+" ^ tt t3 t4 ^ "s\n"); *)
                         result
                       end
                   end

               , after = fn () =>
                   FutharkPoints.free centroids_fut_set
                     Futhark.Real64Array2.free
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
         end
         handle Futhark.Error msg => Util.die ("Futhark error: " ^ msg))
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
               , prefer_gpu = fn device => centroidsChunkGPU arg
               }
         in
           Kmeans.kmeansHybrid centroidsChunk k max_iterations points
         end)
  | _ => Util.die ("unknown -impl " ^ impl)

val (kmeans_iters, kmeans_res) = Benchmark.run ("kmeans " ^ impl) bench

val () = Futhark.Real64Array2.free points_fut

fun writeFile fname s =
  let val os = TextIO.openOut fname
  in TextIO.output (os, s) before TextIO.closeOut os
  end

val () =
  if profile then (writeFile "futhark.json" (Futhark.Context.report ctx))
  else ()

val () = CtxSet.free ctxSet

val () = print ("kmeans iterations: " ^ Int.toString kmeans_iters ^ "\n")
val _ =
  if CLA.parseFlag "print-centroids" then
    ( List.tabulate (k, fn i =>
        print
          (Util.summarizeArraySlice 10 Real.toString (Points.nth kmeans_res i)
           ^ "\n"))
    ; ()
    )
  else
    print "Pass --print-centroids to print results.\n"
