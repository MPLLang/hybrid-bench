(* data initially copied to GPU  a Futhark context for each gpu device *)

functor GpuData (type t) =
struct
  open Device

  type gpu_data = (device_identifier * t) Seq.t

  fun initialize ctxSet f =
    Seq.map (fn (device, ctx) => (device, f ctx)) ctxSet

  fun free data f =
    Seq.map (fn (_, d) => f d) data

  fun choose data device =
    #2 (Seq.first (Seq.filter (fn (d, _) => d = device) data))
end
