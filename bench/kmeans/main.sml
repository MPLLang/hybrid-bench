val () = print ("K-means clustering.\n")

structure CLA = CommandLineArgs

val file = CLA.parseString "points" ""

val d = CLA.parseInt "d" 2

val k = CLA.parseInt "k" 5

val impl = CommandLineArgs.parseString "impl" "cpu"

val () = if d = ~1 then raise Fail "Need -d INT" else ()

val points =
  if file = "" then
    raise Fail "Need -points FILE"
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

val bench =
  case impl of
    "cpu" => (fn () => Kmeans.kmeans k max_iterations points)
  | "gpu" =>
      (fn () =>
         let
           val (num_iters, centroids_fut) =
             Futhark.Entry.kmeans ctx
               (Int64.fromInt k, Int32.fromInt max_iterations, points_fut)
         in
           ( Int32.toInt num_iters
           , Points.fromSeq d (Seq.fromArraySeq
               (ArraySlice.full (Futhark.Real64Array2.values centroids_fut)))
           ) before Futhark.Real64Array2.free centroids_fut
         end)
  | "hybrid" =>
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
  print (Seq.toString Real.toString (Points.nth kmeans_res i) ^ "\n"))
