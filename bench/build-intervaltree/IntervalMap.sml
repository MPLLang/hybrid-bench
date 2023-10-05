structure ITree: Aug =
struct
  type key = Int32.int
  type value = Int32.int
  type aug = Int32.int
  val compare = Int32.compare
  val g = fn (x, y) => y
  val f = fn (x, y) => Int32.max (x, y)
  val id = Int32.fromInt (~1073741824)
  val balance = WB 0.28
  fun debug (k, v, a) =
    Int32.toString k ^ ", " ^ Int32.toString v ^ ", " ^ Int32.toString a
end

signature INTERVAL_MAP =
sig
  type point
  type interval = point * point
  type imap

  val empty: unit -> imap
  val joinMid: imap * interval * imap -> imap

  val interval_map: interval Seq.t -> int -> imap
  val multi_insert: imap -> interval Seq.t -> imap
  val stab: imap -> point -> bool
  val report_all: imap -> point -> imap
  val size: imap -> int
  val print: imap -> unit
end

structure IntervalMap: INTERVAL_MAP =
struct
  structure amap = PAM(ITree)
  type point = ITree.key
  type interval = point * point
  type imap = amap.am

  fun empty () = amap.empty ()

  fun joinMid (im1, (x, y), im2) =
    amap.join im1 x y im2

  fun interval_map s n =
    amap.build s 0 n

  fun multi_insert im s =
    amap.multi_insert im s (Int32.max)

  fun stab im p =
    (amap.aug_left im p) > p

  fun report_all im p =
    amap.aug_filter (amap.up_to im p) (fn q => q > p)

  fun size im = (amap.size im)

  fun print im = amap.print_tree im ""
end
