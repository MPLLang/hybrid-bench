(*
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
    (mandelbrot_package * u8 array, 'a) ForkJoin.gpu_task =
    let
      fun spawn () =
        let
          val data = ForkJoin.alloc ((yhi - ylo) * (bhi - blo))
          val pkg = mandelbrot_spawn (ctx, ylo, yhi, blo, bhi, data)
        in
          (pkg, data)
        end
      fun poll (pkg, _) =
        (mandelbrot_poll pkg = 0w1)
      fun finish (pkg, data) =
        (mandelbrot_finish pkg; data)
    in
      ForkJoin.gpuWithCleanup
        {spawn = spawn, poll = poll, finish = finish, cleanup = cleanup}
    end

end
*)

structure FutMandelbrot =
struct

  type fut_context = FutharkMandelbrot.ctx

  fun init () = FutharkMandelbrot.Context.new FutharkMandelbrot.Config.default

  fun cleanup x = FutharkMandelbrot.Context.free x

  type i64 = Int64.int
  type i32 = Int32.int
  type u8 = Word8.word

  fun mandelbrot ctx ylo yhi blo bhi : u8 array =
    let
      val output = FutharkMandelbrot.Entry.mandelbrot ctx (ylo, yhi, blo, bhi)
      val _ = FutharkMandelbrot.Context.sync ctx
      val arr = FutharkMandelbrot.Word8Array1.values output
    in
      FutharkMandelbrot.Word8Array1.free output;
      arr
    end

end
