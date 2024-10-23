module type S = sig
  type byte = int
  val (&) : byte -> byte -> byte
  val (+) : byte -> byte -> byte
  val shl : byte -> byte
  val shr : byte -> byte
  val (==) : byte -> byte -> bool
  val index : string -> byte -> byte
  val oswrch : byte -> unit
  val osnewl : unit -> unit
end
