module Examples (examples) where

import Language (Exp(..),Form(..),Op1(..),Op2(..))

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

  , xor (add (num 13) (num 2)) (add (num 1) (num 15))
  ]

  where

    num n = Exp (Num n)
    var x = Exp (Var x)
    add e1 e2 = Exp (Op2 Add e1 e2)
    sub e1 e2 = Exp (Op2 Sub e1 e2)
    xor e1 e2 = Exp (Op2 Xor e1 e2)
    asl e = Exp (Op1 Asl e)

    a = var "a"
    x = var "x"
    y = var "y"
    z = var "z"
    z2 = var "z2"