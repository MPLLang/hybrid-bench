structure SegmentedPrimes =
struct

  fun primes_cpu n : Int64.int array array =
    if n < 2 then
      ForkJoin.alloc 0
    else
      let
        val sqrtN = Real.floor (Math.sqrt (Real.fromInt n))
        val sqrtPrimes = primes_cpu sqrtN

        val sqrtPrimes = Seq.flatten
          (Seq.map ArraySlice.full (ArraySlice.full sqrtPrimes))

        (* Split the range [2,n+1) into blocks *)
        val blockSize = Int.max (sqrtN, 1000)
        val numBlocks = Util.ceilDiv ((n + 1) - 2) blockSize

        val (result, tm) = Util.getTime (fn _ =>
          SeqBasis.tabulate 1 (0, numBlocks) (fn b =>
            let
              val lo = 2 + b * blockSize
              val hi = Int.min (lo + blockSize, n + 1)

              val flags = Array.array (hi - lo, 0w1 : Word8.word)
              fun unmark i =
                Array.update (flags, i - lo, 0w0)

              fun loop i =
                if i >= Seq.length sqrtPrimes then
                  ()
                else if 2 * Int64.toInt (Seq.nth sqrtPrimes i) >= hi then
                  ()
                else
                  let
                    val p = Int64.toInt (Seq.nth sqrtPrimes i)
                    val lom = Int.max (2, Util.ceilDiv lo p)
                    val him = Util.ceilDiv hi p
                  in
                    Util.for (lom, him) (fn m => unmark (m * p));
                    loop (i + 1)
                  end

              val _ = loop 0

              val numPrimes = Util.loop (0, hi - lo) 0 (fn (count, i) =>
                if Array.sub (flags, i) = 0w0 then count else count + 1)

              val output = ForkJoin.alloc numPrimes

              val _ = Util.loop (lo, hi) 0 (fn (outi, i) =>
                if Array.sub (flags, i - lo) = 0w0 then outi
                else (Array.update (output, outi, Int64.fromInt i); outi + 1))
            in
              output
            end))

        val _ = print
          ("sieve (n=" ^ Int.toString n ^ "): " ^ Time.fmt 4 tm ^ "s\n")
      in
        result
      end

end
