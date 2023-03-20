structure FutRay =
struct

  fun init () =
    (_import "fut_init" public : unit -> MLton.Pointer.t;) ()

  fun cleanup x =
    (_import "fut_cleanup" public : MLton.Pointer.t -> unit;) x

  type i64 = Int64.int
  type i32 = Int32.int

  val render_spawn =
    _import "render_spawn" public : MLton.Pointer.t * i64 * i64 * i64 * i64 * ( i32 array ) -> MLton.Pointer.t;

  val render_poll =
    _import "render_poll" public : MLton.Pointer.t -> Word8.word;

  val render_finish = _import "render_finish" public : MLton.Pointer.t -> unit;

  fun render ctx height width output =
    let
      val (data, start, len) = ArraySlice.base output

      fun spawn () =
        render_spawn (ctx, height, width, start, len, data)
      fun poll x =
        (render_poll x = 0w1)
      fun finish x = render_finish x
    in
      ForkJoin.gpu {spawn = spawn, poll = poll, finish = finish}
    end

end
