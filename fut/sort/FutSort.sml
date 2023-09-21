structure FutSort =
struct

  type fut_context = MLton.Pointer.t

  fun init () =
    (_import "fut_init" public : unit -> fut_context;) ()

  fun cleanup x =
    (_import "fut_cleanup" public : fut_context -> unit;) x

  type i64 = Int64.int
  type i32 = Int32.int

  type sort_package = MLton.Pointer.t

  val sort_spawn =
    _import "sort_spawn" public : fut_context * i32 array * i64 * i64 * i32 array -> sort_package;

  (* val sort_poll = _import "sort_poll" public : sort_package -> Word8.word; *)

  val sort_finish = _import "sort_finish" public : sort_package -> unit;

  fun sort ctx seq : i32 Seq.t =
    let
      val (data, start, len) = ArraySlice.base seq
      val output = ForkJoin.alloc len
      val pkg = sort_spawn (ctx, data, start, len, output)
    in
      sort_finish pkg;
      ArraySlice.full output
    end

end
