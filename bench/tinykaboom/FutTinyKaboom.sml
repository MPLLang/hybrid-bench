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

  type render_pixels_package = MLton.Pointer.t

  val render_pixels_spawn =
    _import "render_pixels_spawn" public : fut_context * i64 * i64 * f32 * i64 * i64 * u32 array -> render_pixels_package;

  (* 
    val render_pixels_poll =
      _import "render_pixels_poll" public : fut_context -> Word8.word; *)

  val render_pixels_finish =
    _import "render_pixels_finish" public : fut_context -> unit;


  fun render_pixels ctx width height t output : unit =
    let
      val (data, start, len) = ArraySlice.base output
      val lo = start
      val hi = start + len
      (* val _ = print
        ("calling render_pixels_spawn " ^ Int.toString lo ^ " "
         ^ Int.toString hi ^ "\n") *)
      val pack = render_pixels_spawn (ctx, width, height, t, lo, hi, data)
    in
      render_pixels_finish pack
    end

end
