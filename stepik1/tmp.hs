module Monoid where
-- stepik 4.6


import Prelude hiding (lookup)
import qualified Data.List as L
import qualified Data.Char as C



newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Monoid Xor where
    mempty = Xor False
instance Semigroup Xor where
    Xor x <> Xor y = Xor ((x || y) && (not x || not y))


{-
data Maybe a = Nothing | Just a

instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing
instance Semigroup a => Semigroup (Maybe a) where
  Nothing <> m = mempty
  m <> Nothing = mempty
  Just m1 <> Just m2 = Just (m1 <> m2)
-}

newtype First a = First {getFirst :: Maybe a}
  deriving (Show, Read, Ord, Eq)
instance Monoid (First a) where
  mempty = First Nothing
instance Semigroup (First a) where
  First Nothing <> r = r
  l <> _             = l

newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)
instance Monoid a => Monoid (Maybe' a) where
  mempty = Maybe' (Just mempty)  -- Maybe' Nothing
instance Semigroup a => Semigroup (Maybe' a) where
  Maybe' Nothing <> _ = Maybe' Nothing
  _ <> Maybe' Nothing = Maybe' Nothing
  Maybe' x <> Maybe' y = Maybe' (x <> y)


{-
class MapLike a where
    empty :: a k v
    lookup :: Ord k => k -> a k v -> Maybe v
    insert :: Ord k => k -> v -> a k v -> a k v
    delete :: Ord k => k -> a k v -> a k v
    fromList :: Ord k => [(k,v)] -> a k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)
    
instance MapLike ListMap where
    empty = ListMap []
    lookup x (ListMap []) = Nothing
    lookup x (ListMap ((k,v):xs)) = if x == k then Just v else lookup x (ListMap xs)
    insert x y (ListMap xs) = ListMap (f x y xs) where
        f x y [] = [(x,y)]
        f x y ((k,v) :xs) = if x == k then (k,y):xs else (k,v): f x y xs
    delete x (ListMap xs) = ListMap (f x xs) where
        f x [] = []
        f x ((k,v) :xs) = if x == k then xs else (k,v): f x xs

x = ListMap [(1, "first"),(2, "second"),(3, "therd")]
-}


data Entry k1 k2 v = Entry (k1, k2) v
instance (Show k1, Show k2, Show v) => Show (Entry k1 k2 v) where
  show (Entry (a1,a2) b) = filter (/='"') $ show a1 ++ "x" ++ show a2 ++ " - " ++ show b

data Map k1 k2 v = Map [Entry k1 k2 v]
instance (Show k1, Show k2, Show v) => Show (Map k1 k2 v) where
  show (Map a) = concatMap show a


instance Functor (Entry k1 k2) where
    fmap f (Entry k x) = Entry k (f x)

instance Functor (Map k1 k2) where
--  fmap f (Map xs) = Map [fmap f x | x <- xs]
    fmap f (Map xs) = Map (fmap (fmap f) xs)


x = fmap id $ Map [Entry (0, 0) "origin", Entry (800, 0) "right corner"]













