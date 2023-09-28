structure CLA = CommandLineArgs

val ctx = FutharkPrimes.ctx_new FutharkPrimes.default_cfg

(* ==========================================================================
 * primes on cpu
 *)

fun primes_cpu n =
  if n < 2 then
    ForkJoin.alloc 0
  else
    let
      val sqrtN = Real.floor (Math.sqrt (Real.fromInt n))
      val sqrtPrimes = primes_cpu sqrtN

      val flags = ForkJoin.alloc (n + 1) : Word8.word array
      fun mark i = Array.update (flags, i, 0w0)
      fun unmark i = Array.update (flags, i, 0w1)
      fun isMarked i =
        Array.sub (flags, i) = 0w0
      val _ = ForkJoin.parfor 10000 (0, n + 1) mark

      val blockSize = Int.max (sqrtN, 1000)
      val numBlocks = Util.ceilDiv (n + 1) blockSize

      val _ = ForkJoin.parfor 1 (0, numBlocks) (fn b =>
        let
          val lo = b * blockSize
          val hi = Int.min (lo + blockSize, n + 1)

          fun loop i =
            if i >= Array.length sqrtPrimes then
              ()
            else if 2 * Array.sub (sqrtPrimes, i) >= hi then
              ()
            else
              let
                val p = Array.sub (sqrtPrimes, i)
                val lom = Int.max (2, Util.ceilDiv lo p)
                val him = Util.ceilDiv hi p
              in
                Util.for (lom, him) (fn m => unmark (m * p));
                loop (i + 1)
              end
        in
          loop 0
        end)
    in
      SeqBasis.filter 4096 (2, n + 1) (fn i => i) isMarked
    end

(* ==========================================================================
 * primes on gpu
 *)


fun primes_gpu n =
  let
    (* this seems wrong. `count` shouldn't be available until AFTER ctx_sync *)
    val (farr, count) = FutharkPrimes.Entry.primes ctx n
    val _ = FutharkPrimes.ctx_sync ctx
  in
    ()
  end
