data Nat = Zero | Suc Nat
  deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat x = Suc (toNat (x - 1))

add :: Nat -> Nat -> Nat
add a Zero = a
add a (Suc n) = add (Suc a) n

mul :: Nat -> Nat -> Nat
mul a Zero = Zero
mul a (Suc Zero) = a
mul a (Suc n) = add (mul a n) a

-- 7 3 - 7+7 2 - 7+7 1 - 

fac :: Nat -> Nat
fac Zero = Suc Zero
fac p@(Suc n) = mul p (fac n)

fac' = toNat . fact . fromNat

fact :: Integer -> Integer
fact = product . flip take [1..] . fromIntegral