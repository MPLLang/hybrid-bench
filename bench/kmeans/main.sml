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
      Points.fromArraySeq d s
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

val (kmeans_iters, kmeans_res) = Kmeans.kmeans k points max_iterations

val () = print ("kmeans iterations: " ^ Int.toString kmeans_iters ^ "\n")
val _ = List.tabulate (k, fn i =>
  print (Seq.toString Real.toString (Points.nth kmeans_res i) ^ "\n"))
