open Printf
module G = Gen6502examples
module Prim : G.Prim.S = struct
  type byte = int
  let byte n = n mod 256
  let (+) = (+)
  let (&) = (land)
  let shl x = byte (2*x)
  let shr x = x/2
  let (==) = (=)
  let index s x = Char.code (s.[x])
  let oswrch x = printf "%c" (Char.chr x)
  let osnewl () = printf "\n"
end
let () = printf "Collatz.main()...\n"
module Example = G.First (*Collatz*)
module M = Example.F(Prim)
let main = M.main
let res = main ()
let () = printf "res=%d\n" res
