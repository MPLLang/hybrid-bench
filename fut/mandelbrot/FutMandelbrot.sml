structure FutMandelbrot =
struct

  type fut_context = MLton.Pointer.t

  fun init () =
    (_import "fut_init" public : unit -> fut_context;) ()

  fun cleanup x =
    (_import "fut_cleanup" public : fut_context -> unit;) x

  type i64 = Int64.int
  type i32 = Int32.int
  type u8 = Word8.word

  type mandelbrot_package = MLton.Pointer.t

  val mandelbrot_spawn =
    _import "mandelbrot_spawn" public : fut_context * i64 * i64 * i64 * i64 * u8 array -> mandelbrot_package;

  val mandelbrot_poll =
    _import "mandelbrot_poll" public : mandelbrot_package -> Word8.word;

  val mandelbrot_finish =
    _import "mandelbrot_finish" public : mandelbrot_package -> unit;

  fun mandelbrot ctx ylo yhi blo bhi (cleanup: u8 array -> 'a) :
    (mandelbrot_package, 'a) ForkJoin.gpu_task =
    let
      val data = ForkJoin.alloc ((yhi - ylo) * (bhi - blo))

      fun spawn () =
        mandelbrot_spawn (ctx, ylo, yhi, blo, bhi, data)
      fun poll pkg =
        (mandelbrot_poll pkg = 0w1)
      fun finish pkg =
        (mandelbrot_finish pkg; data)
    in
      ForkJoin.gpuWithCleanup
        {spawn = spawn, poll = poll, finish = finish, cleanup = cleanup}
    end

end
