(* maintain a Futhark context for each gpu device *)

structure CtxSet:
sig
  type device_identifier = string
  type ctx_set = (device_identifier * FutharkSort.ctx) Seq.t
  val fromList: device_identifier list -> ctx_set
  val choose: ctx_set -> device_identifier -> FutharkSort.ctx
end =
struct
  type device_identifier = string
  type ctx_set = (device_identifier * FutharkSort.ctx) Seq.t

  fun fromList (devices: device_identifier list) =
    Seq.map
      (fn name =>
         ( name
         , FutharkSort.Context.new
             (FutharkSort.Config.cache (SOME "futhark.cache")
                (FutharkSort.Config.device (SOME name) FutharkSort.Config.default))
         )) (Seq.fromList devices)

  fun free (ctxMap: ctx_set) =
    Seq.map (fn (_, ctx) => FutharkSort.Context.free ctx) ctxMap

  fun choose (ctxMap: ctx_set) (device: device_identifier) =
  let
    val (device, ctx) = Seq.first (Seq.filter (fn (name, _) => name = device) ctxMap)
    val _ = print ("chosen device: " ^ device ^ "\n")
  in
  ctx
  end
end
