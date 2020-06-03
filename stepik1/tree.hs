module BinaryTree where
-- stepic 4.5.3

data Tree a = Leaf a | Node (Tree a) (Tree a)
 deriving Show

height :: Tree a -> Int
height tree = case tree of
    Leaf a -> 0
    Node x y -> max (height x + 1) (height y + 1)

size :: Tree a -> Int
size tree = case tree of
    Leaf a -> 1
    Node x y -> 1 + size x + size y

go :: Tree Int -> (Int,Int)
go tree = case tree of
    Leaf a -> (1, a)
    Node x y -> (a1+a2, b1+b2) where (a1, b1) = go x; (a2, b2) = go y 

tree0 :: Tree Int
tree0 = Leaf 1
tree1 = Node (Leaf 2) (Leaf 2) :: Tree Int
tree2 = Node (Node (Leaf 1) (Leaf 1)) (Node (Leaf 2) (Leaf 2)) :: Tree Int
treeX = Node -- 275 11
  (Node 
    (Leaf 17)
    (Leaf 21)
  ) 
  (Node
    (Node
      (Leaf 36) 
      (Node
        (Leaf 27)
        (Node
          (Node
            (Leaf 15)
            (Node
            (Leaf 3)
            (Leaf 19)
            )
          )
          (Leaf 56)
        )
      )
    ) 
    (Node 
      (Node 
        (Leaf 8)
        (Leaf 31)
      )
      (Leaf 42)
    )
  ) :: Tree Int




infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

a :: Expr
a = Val 1 :*: (Val 2 :+: Val 3) :*: Val 4


expand' :: Expr -> Expr
expand' ((e1 :+: e2) :*: e) = expand' e1 :*: expand' e :+: expand' e2 :*: expand' e
expand' (e :*: (e1 :+: e2)) = expand' e :*: expand' e1 :+: expand' e :*: expand' e2
expand' (e1 :+: e2) = expand' e1 :+: expand' e2
expand' (e1 :*: e2) = expand' e1 :*: expand' e2
expand' e = e

expand x = if a == expand' a
            then a
            else expand a 
            where a = expand' x



























