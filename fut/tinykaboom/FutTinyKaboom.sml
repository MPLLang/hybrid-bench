structure FutTinyKaboom =
struct

  type fut_context = MLton.Pointer.t

  fun init () =
    (_import "fut_init" public : unit -> fut_context;) ()

  fun cleanup x =
    (_import "fut_cleanup" public : fut_context -> unit;) x

  type i64 = Int64.int
  type i32 = Int32.int
  type f32 = Real32.real
  type u32 = Word32.word

  type compute_frame_package = MLton.Pointer.t

  val compute_frame_spawn =
    _import "compute_frame_spawn" public : fut_context * i64 * i64 * f32 * u32 array -> compute_frame_package;

  val compute_frame_poll =
    _import "compute_frame_poll" public : fut_context -> Word8.word;

  val compute_frame_finish =
    _import "compute_frame_finish" public : fut_context -> unit;

  fun compute_frame ctx width height t :
    (compute_frame_package, u32 array) ForkJoin.gpu_task =
    let
      val output = ForkJoin.alloc (width * height)

      fun spawn () =
        compute_frame_spawn (ctx, width, height, t, output)
      fun poll x =
        (compute_frame_poll x = 0w1)
      fun finish x =
        (compute_frame_finish x; output)
    in
      ForkJoin.gpu {spawn = spawn, poll = poll, finish = finish}
    end

end
