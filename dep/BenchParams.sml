structure BenchParams =
struct

  structure Mandelbrot =
  struct
    (* One big hybrid parfor over the rows. Split in half, and hybridization
     * threshold at a grain of 10 rows.
     *)
    val parfor_split = 0.5
    val parfor_grain = 10
  end

end
