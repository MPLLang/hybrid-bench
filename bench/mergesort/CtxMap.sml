structure CtxMap:
sig
  type device_identifier = string
  type ctx_map = (device_identifier * FutharkSort.ctx) Seq.t
  val fromList: device_identifier list -> ctx_map
  val choose: ctx_map -> device_identifier -> FutharkSort.ctx
end =
struct
  type device_identifier = string
  type ctx_map = (device_identifier * FutharkSort.ctx) Seq.t

  fun fromList (devices: device_identifier list) =
    Seq.map
      (fn name =>
         ( name
         , FutharkSort.Context.new
             (FutharkSort.Config.cache (SOME "futhark.cache")
                (FutharkSort.Config.device (SOME name) FutharkSort.Config.default))
         )) (Seq.fromList devices)

  fun free (ctxMap: ctx_map) =
    Seq.map (fn (_, ctx) => FutharkSort.Context.free ctx) ctxMap

  fun choose (ctxMap: ctx_map) (device: device_identifier) =
    #2 (Seq.first (Seq.filter (fn (name, _) => name = device) ctxMap))

end
