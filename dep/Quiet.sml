(* use --quiet at the command line to disable these prints *)
structure Quiet:
sig
  val print: (unit -> string) -> unit
  val println: (unit -> string) -> unit
end =
struct

  val beQuiet = CommandLineArgs.parseFlag "quiet"

  fun print_ f =
    if beQuiet then () else print (f ())

  fun println_ f =
    if beQuiet then () else print (f () ^ "\n")


  val print = print_
  val println = println_

end