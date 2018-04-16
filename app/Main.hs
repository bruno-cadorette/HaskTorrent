{-# LANGUAGE
RankNTypes,
TupleSections
  #-}

module Main where
import Networking
import File
import Data.Functor.Identity

import Control.Applicative



type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

view :: Lens s t a b -> s -> a
view lens s = getConst $ lens (\x -> Const x) s

over :: Lens s t a b -> (a -> b) -> s -> t
over lens f = runIdentity . lens (pure . f)

set :: Lens s t a b -> b -> s -> t
set lens b = over lens (const b)

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 = \f (a, x) -> fmap (\a' -> (a', x)) $ f a

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 = \f (x, a) -> fmap (\a' -> (x, a')) $ f a

-- Make a lens out of a getter and a setter.
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = \f s -> fmap (set s) $ f (get s)

-- Combine 2 lenses to make a lens which works on Either. (It's a good idea
-- to try to use bimap for this, but it won't work, and you have to use
-- explicit case-matching. Still a good idea, tho.)
choosing :: Lens s1 t1 a b -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 = lens 
    (\s -> 
        case s of
            Left s1 -> view l1 s1
            Right s2 -> view l2 s2
    )
    (\s b -> 
        case s of
            Left s1 -> Left $ set l1 b s1
            Right s2 -> Right $ set l2 b s2)


-- Modify the target of a lens and return the result. (Bonus points if you
-- do it without lambdas and defining new functions. There's also a hint
-- before the end of the section, so don't scroll if you don't want it.)
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = (f (view l s), runIdentity $ l (\a -> pure $ f a) s)

-- Modify the target of a lens, but return the old value.
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = ((view l s), runIdentity $ l (\a -> pure $ f a) s)

-- There's a () in every value. (No idea what this one is for, maybe it'll
-- become clear later.)
united :: Lens' s () 
united =  \f s -> s <$ f ()




main :: IO ()
main = mainNetwork
