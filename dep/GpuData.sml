(* data initially copied to GPU  a Futhark context for each gpu device *)

structure GpuData:
sig
  type 'a gpu_data = (Device.device_identifier * 'a) Seq.t
  type 'a t = 'a gpu_data
  val initialize: (Device.device_identifier * 'ctx) Seq.t
                  -> ('ctx -> 'a)
                  -> 'a gpu_data
  val free: 'a gpu_data -> ('a -> unit) -> unit
  val choose: 'a gpu_data -> Device.device_identifier -> 'a
end =
struct
  open Device

  type 'a gpu_data = (device_identifier * 'a) Seq.t
  type 'a t = 'a gpu_data

  fun initialize ctxSet f =
    Seq.map (fn (device, ctx) => (device, f ctx)) ctxSet

  fun free data f =
    let val _ = Seq.map (fn (_, d) => f d) data
    in ()
    end

  fun choose data device =
    #2 (Seq.first (Seq.filter (fn (d, _) => d = device) data))
end
