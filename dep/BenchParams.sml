structure BenchParams =
struct

  structure Mandelbrot =
  struct
    (* One big asymmetric hybrid parfor over the rows. Split in half, and
     * hybridization threshold at a grain of 10 rows.
     *)
    val parfor_split = CommandLineArgs.parseReal "mandelbrot-parfor-split" 0.5
    val parfor_grain = CommandLineArgs.parseInt "mandelbrot-parfor-grain" 10
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


  structure Mergesort =
  struct

    (* ==================================================================
     * algorithmic parameters
     *)

    val qsort_grain = CommandLineArgs.parseInt "qsort-grain" 100000

    val gpu_merge_block = CommandLineArgs.parseInt "gpu-merge-block" 8


    (* ==================================================================
     * hybridization parameters
     *
     * for sort: 
     *   - sort_split: midpoint for asymmetric divide-and-conquer. Increasing
     *     this parameter gives more work to the GPU (e.g., 0.25 means split
     *     into 25%/75% and put a hybrid task on the 25% piece)
     *   - sort_grain: below this threshold, stop hybridizing
     *
     * for merging (subroutine of the sort): very similar
     *   - merge_split: midpoint for asymmetric divide-and-conquer
     *   - merge_grain: below this threshold, stop hybridizing
     *)

    val sort_split = CommandLineArgs.parseReal "sort-split" 0.36
    val sort_grain = CommandLineArgs.parseInt "sort-grain" 1500000

    val merge_split = CommandLineArgs.parseReal "merge-split" 0.15
    val merge_grain = CommandLineArgs.parseInt "merge-grain" 1500000

  end


  structure Kmeans =
  struct

    (* ======================================================================
     * hybridization
     *
     * Large fanout with asymmetric divide-and-conquer at the leaves. Size
     * of the leaves is controlled by hist_gpu_split (0 < ... < 1) and we
     * switch to cpu-only below hist_gpu_grain.
     *
     * hist_cpu_grain is used purely for CPU-side grain control.
     *)

    val hist_cpu_grain = CommandLineArgs.parseInt "hist-cpu-grain" 1000
    val hist_gpu_grain = CommandLineArgs.parseInt "hist-gpu-grain" 100000
    val hist_gpu_split = CommandLineArgs.parseReal "hist-gpu-split" 0.75

  end


  structure Raytracer =
  struct

    (* ======================================================================
     * hybridization
     *
     * Simple hybrid parfor. Split in half, and stop hybridizing below the
     * grain size. 
     *)

    val gpu_split = CommandLineArgs.parseReal "render-hybrid-gpu-split" 0.5
    val gpu_grain = 2000

  end


  structure Quickhull =
  struct

    (* ======================================================================
     * hybridization
     *
     * semihull: asymmetric divide-and-conquer
     * 
     * lots of subroutines hybridized, including reduces and filters, all of
     * which use the `reduce_hybrid_grain` and `reduce_hybrid_split`
     *)

    val semihull_par_grain = 1000
    val semihull_hybrid_grain = 500

    val reduce_hybrid_grain = 5000
    val reduce_hybrid_split = 0.5

  end


  structure Bfs =
  struct
    val sparse_hybrid_threshold =
      CommandLineArgs.parseReal "bfs-sparse-hybrid-threshold" 1.0
    val dense_hybrid_split =
      CommandLineArgs.parseReal "bfs-dense-hybrid-split" 0.25
    val sparse_hybrid_split =
      CommandLineArgs.parseReal "bfs-sparse-hybrid-split" 0.2
  end


  structure SparseMxv =
  struct
    val nnz_grain = CommandLineArgs.parseInt "matcoo-nnz-grain" 5000
    val nnz_grain_hybrid =
      CommandLineArgs.parseInt "matcoo-nnz-grain-hybrid" (1000 * 1000)
    val hybrid_split = CommandLineArgs.parseReal "matcoo-hybrid-split" 0.15
    val gpu_block_size = CommandLineArgs.parseInt "matcoo-gpu-block-size"
      (10 * 1000 * 1000)

    val hybrid_gpu_work_rat =
      CommandLineArgs.parseReal "matcoo-hybrid-gpu-work-rat" 20.0
  end

end
