(* maintain a Futhark context for each gpu device *)

functor CtxSetFn
  (structure F:
   sig
     type ctx
     exception Error of string
     type cfg =
       { logging: bool
       , debugging: bool
       , profiling: bool
       , cache: string option
       , tuning: (string * int) list
       , device: string option
       , unified_memory: int option
       }

     structure Config:
     sig
       val default: cfg
       val logging: bool -> cfg -> cfg
       val debugging: bool -> cfg -> cfg
       val profiling: bool -> cfg -> cfg
       val cache: string option -> cfg -> cfg
       val tuning: (string * int) list -> cfg -> cfg
       val device: string option -> cfg -> cfg
       val unified_memory: int option -> cfg -> cfg
     end

     structure Context:
     sig
       val new: cfg -> ctx
       val free: ctx -> unit
     end
   end): CTX_SET where type ctx = F.ctx =
struct
  open Device

  val profile = CommandLineArgs.parseFlag "profile"
  val logging = CommandLineArgs.parseFlag "logging"

  type ctx = F.ctx
  type ctx_set = (device_identifier * F.ctx) Seq.t
  type t = ctx_set

  fun fromList (devices: device_identifier list) =
    Seq.map
      (fn device =>
         let
           val cfg =
             (F.Config.cache (SOME ("futhark.cache"))
              o F.Config.device (SOME (device ^ String.implode [Char.chr 0])) o F.Config.profiling profile
              o F.Config.logging logging o F.Config.unified_memory (SOME 0))
               F.Config.default
           val ctx = F.Context.new cfg
         in
           (device, ctx)
         end) (Seq.fromList devices)

  fun free (ctxSet: ctx_set) =
    let val _ = Seq.map (fn (_, ctx) => F.Context.free ctx) ctxSet
    in ()
    end


  fun choose (ctxSet: ctx_set) (device: device_identifier) =
    let
      val (device, ctx) = Seq.first
        (Seq.filter (fn (d, _) => d = device) ctxSet)
      (*val _ = print ("chosen device: " ^ device ^ "\n")*)
    in
      ctx
    end

  fun toCtxList (ctxSet: ctx_set) =
    Seq.toList (Seq.map (fn (_, ctx) => ctx) ctxSet)
end
