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
       val report: ctx -> string
       val pauseProfiling: ctx -> unit
       val unpauseProfiling: ctx -> unit
     end
   end): CTX_SET where type ctx = F.ctx =
struct
  open Device

  val profile = CommandLineArgs.parseFlag "profile"
  val logging = CommandLineArgs.parseFlag "logging"

  type ctx = F.ctx
  type ctx_set = (gpu_identifier * F.ctx) Seq.t
  type t = ctx_set

  fun fromList (devices: gpu_identifier list) =
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

  fun writeFile fname s =
    let val os = TextIO.openOut fname
    in TextIO.output (os, s) before TextIO.closeOut os
    end

  fun toCtxList (ctxSet: ctx_set) =
    Seq.toList (Seq.map (fn (_, ctx) => ctx) ctxSet)

  fun report (ctxSet: ctx_set) =
    if profile then
        let
          val _ =
            List.foldl
              (fn (ctx, idx) =>
                 let
                   val fname = "futhark" ^ (Int.toString idx) ^ ".json"
                   val () = print ("Writing " ^ fname ^ "\n")
                   val _ =
                     (writeFile fname
                        (F.Context.report ctx))
                 in
                   idx + 1
                 end) 0 (toCtxList ctxSet)
        in
          ()
        end
      else
        ()

  fun free (ctxSet: ctx_set) =
    let val () = report ctxSet
        val _ = Seq.map (fn (_, ctx) => F.Context.free ctx) ctxSet
    in ()
    end

  fun choose (ctxSet: ctx_set) (device: device_identifier) =
    let
    (* val () = print ("device is " ^ device ^ "\n") *)
    val (_, ctx) = Seq.nth ctxSet device
      (* val (device, ctx) = Seq.first
        (Seq.filter (fn (d, _) => d = device) ctxSet) *)
      (*val _ = print ("chosen device: " ^ device ^ "\n")*)
    in
      ctx
    end

  fun pauseProfiling (ctxSet: ctx_set) =
    List.app F.Context.pauseProfiling (toCtxList ctxSet)

  fun unpauseProfiling (ctxSet: ctx_set) =
    List.app F.Context.unpauseProfiling (toCtxList ctxSet)
end
