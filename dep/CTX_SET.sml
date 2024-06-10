signature CTX_SET =
sig
  type device_identifier = Device.device_identifier
  type ctx
  type ctx_set = (device_identifier * ctx) Seq.t
  type t = ctx_set

  val fromList: device_identifier list -> ctx_set
  val choose: ctx_set -> device_identifier -> ctx
  val toCtxList: ctx_set -> ctx list
  val free: ctx_set -> unit
end
