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
     end

     structure Context:
     sig
       val new: cfg -> ctx
       val free: ctx -> unit
     end
   end) =
struct
  open Device

  val profile = CommandLineArgs.parseFlag "profile"

  type ctx_set = (device_identifier * F.ctx) Seq.t

  fun fromList (devices: device_identifier list) =
    Seq.map
      (fn device =>
         let
           val cfg =
             (F.Config.cache (SOME "futhark.cache")
              o F.Config.device (SOME device) o F.Config.profiling profile)
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
      val _ = print ("chosen device: " ^ device ^ "\n")
    in
      ctx
    end
end
