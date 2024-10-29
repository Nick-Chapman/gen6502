module Examples (examples) where

import Program

examples :: [Exp]
examples =
  [ num 77
  , a
  , x
  , y
  , z

  , add a (num 1)
  , add x (num 1)
  , add y (num 1)
  , add z (num 1)
  , add (num 1) a
  , add (num 1) x
  , add (num 1) y
  , add (num 1) z
  , add a (num 2)
  , add x (num 2)
  , add y (num 2)
  , add z (num 2)
  , add (num 3) a
  , add (num 3) x
  , add (num 3) y
  , add (num 3) z
  , add a a
  , add a x
  , add a y
  , add a z
  , add x a
  , add x x
  , add x y
  , add x z
  , add y a
  , add y x
  , add y y
  , add y z
  , add z a
  , add z x
  , add z y
  , add z z
  , add z z2
  , add (num 17) (num 19)
  , add (num 14) (num 1)
  , add (num 1) (num 14)

  , sub a (num 1)
  , sub x (num 1)
  , sub y (num 1)
  , sub z (num 1)
  , sub (num 1) a
  , sub (num 1) x
  , sub (num 1) y
  , sub (num 1) z
  , sub a (num 2)
  , sub x (num 2)
  , sub y (num 2)
  , sub z (num 2)
  , sub (num 3) a
  , sub (num 3) x
  , sub (num 3) y
  , sub (num 3) z
  , sub a a
  , sub a x
  , sub a y
  , sub a z
  , sub x a
  , sub x x
  , sub x y
  , sub x z
  , sub y a
  , sub y x
  , sub y y
  , sub y z
  , sub z a
  , sub z x
  , sub z y
  , sub z z
  , sub z z2
  , sub (num 17) (num 19)
  , sub (num 14) (num 1)
  , sub (num 1) (num 14)

  , asl (num 14)
  , asl a
  , asl x
  , asl y
  , asl z

  , add (asl a) a
  , add (asl x) x
  , add (asl y) y
  , add (asl z) z

  , add (add a (num 1)) (add a (num 1))
  , add (add (num 17) (num 19)) (add (num 17) (num 19))

  , xor a (asl a)
  , xor a (asl (asl a))

  , xor (add a (num 1)) (add a (num 1))
  , xor (add x (num 1)) (add x (num 1))
  , xor (add y (num 1)) (add y (num 1))
  , xor (add z (num 1)) (add z (num 1))

  , xor (xor a (asl a)) (asl (asl a))

  , xor (xor (asl x) (asl y)) (xor (asl z) (asl a))

  , add (add (add a x) (add y z)) (add (add x y) (add z a))
  , xor (xor (xor a x) (xor y z)) (xor (xor x y) (xor z a))

  , xor (add (num 13) z) (add (num 1) (num 13))
  , xor (add (num 13) (num 2)) (add (num 1) (num 13))
  , xor (add z (num 13)) (add (num 1) (num 13))
  , xor (add (num 2) (num 13)) (add (num 1) (num 13))

  , xor (add x y) (add x y) -- syntactic CSE
  , xor (add x y) (add y x) -- semantic CSE

  , Let "xy" (add x y) (xor (var "xy") (var "xy"))

  , Ite (equal x y) z z2
  , asl (Ite (equal x y) z z2)

  , Ite (equal a x) z (add (num 1) z)

  , add (Ite (equal a x) a x) (num 1)
  , asl (add (Ite (equal a x) a x) (num 5))
  , asl (add (Ite (equal a x) a x) (num 1))
  , asl (asl (add (Ite (equal a x) a x) (num 5)))
  -- , asl (asl (add (Ite (equal a x) a x) (num 1)))

  , Ite (equal x x) z z2

  -- TODO: add collatz(step) examples + more!
  ]

  where
    num n = Num n
    var x = Var x
    add e1 e2 = App "+" [e1,e2]
    sub e1 e2 = App "-" [e1,e2]
    xor e1 e2 = App "^" [e1,e2]
    equal e1 e2 = App "==" [e1,e2]
    asl e = App "shl" [e]

    a = var "a"
    x = var "x"
    y = var "y"
    z = var "z"
    z2 = var "z2"

