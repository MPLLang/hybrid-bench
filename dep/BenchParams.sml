structure BenchParams =
struct

  structure Mandelbrot =
  struct
    (* One big asymmetric hybrid parfor over the rows. Split in half, and
     * hybridization threshold at a grain of 10 rows.
     *)
    val parfor_split = 0.5
    val parfor_grain = 10
  end


  structure Primes =
  struct
    (* Algorithmic parameter (not hybridization...)
     *
     * Increasing the block size factor will use larger blocks, which has all
     * of the following effects on performance:
     *   (1) decreased theoretical work, but also worse data locality
     *   (2) less parallelism
     *)
    val block_size_factor =
      CommandLineArgs.parseReal "primes-block-size-factor" 8.0

    (* ======================================================================
     * hybridization parameters
     *
     * The algorithm logically divides the output into blocks. The
     * hybridization in this case is essentially a hybrid parfor over the
     * blocks, but with larger fanout.
     *   - `0 < hybrid_split < 1` controls the fanout: increasing hybrid_split 
     *     will increase the number of blocks in each leaf (and therefore
     *     decrease the fanout).
     *   - Each leaf is a set of blocks, and within each leaf, we do the
     *     typical asymmetric split-in-half hybrid parfor to process blocks
     *     in parallel. Blocks do not have uniform cost, but we can easily
     *     estimate the cost of a set of contiguous blocks. The parameter
     *     `block_range_hybrid_threshold` controls the hybridization parameter
     *     for parallelism within a leaf -- sets of blocks that together have
     *     cost less than `block_range_hybrid_threshold` will be processed on
     *     CPU.
     *)

    val hybrid_split = CommandLineArgs.parseReal "hybrid-gpu-split" 0.025
    val block_range_hybrid_threshold = 500000
  end

end
