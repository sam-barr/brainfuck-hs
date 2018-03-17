module Tape (Tape (..), moveR, moveL, val) where

import Control.Monad
import Control.Comonad
import Data.Monoid
import Data.List (intersperse)

-- Tape datatype
data Tape a = T [a] a [a] deriving (Eq)

instance Show a => Show (Tape a) where
  show (T l v r) = lstring ++ " [" ++ show v ++ "] " ++ rstring
    where
      lstring = listShow $ reverse $ take 10 l
      rstring = listShow $ take 10 r

-- used to print the tape
listShow :: Show a => [a] -> String
listShow [] = ""
listShow x  = intersperse ' ' $ (x >>= show)

-- value of head of tape
val :: Tape a -> a
val (T _ v _) = v

-- moves the tape right
moveR :: Tape a -> Tape a
moveR (T l v (r:rs)) = T (v:l) r rs
moveR _ = error "No more tape"

-- moves the tape left
moveL :: Tape a -> Tape a
moveL (T (l:ls) v r) = T ls l (v:r)
moveL _ = error "No more tape"

-- Tape instances that I don't use

instance Functor Tape where
  fmap f (T l v r) = T (map f l) (f v) (map f r)

instance Comonad Tape where
  extract = val

  duplicate t = T (iterate moveL t) t (iterate moveR t)

instance Foldable Tape where
  foldMap f (T l v r) = f v <> foldMap f l <> foldMap f r
