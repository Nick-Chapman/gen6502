module type S = sig
  type byte = int
  val oswrch : byte -> unit
  val index : string -> byte -> byte
  val (&) : byte -> byte -> byte
  val (+) : byte -> byte -> byte
  val shl : byte -> byte
  val shr : byte -> byte
  val (==) : byte -> byte -> bool
end
