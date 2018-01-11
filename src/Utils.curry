--- ----------------------------------------------------------------------------
--- This module provides some utility operations.
---
--- @author  Jan Tikovsky
--- @version January 2018
--- ----------------------------------------------------------------------------
module Utils where

import Directory (getCurrentDirectory, setCurrentDirectory)
import FiniteMap

import Text.Pretty

-- some useful combinators for monads
infixl 4 <$>, <*>

-- TODO: Remove when there are corresponding functions in the libraries
zipWithM_ :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ f xs ys = sequence_ $ zipWith f xs ys

--- Apply a pure function to the result of a monadic action.
(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) f act = act >>= \x -> return (f x)

--- Apply a function originating from the first monadic computation
--- to the result of the second monadic action.
(<*>) :: Monad m => m (a -> b) -> m a -> m b
a <*> b = a >>= \f -> b >>= \x -> return (f x)

--- Pad a string to a specific length with space from the right side.
rpad :: Int -> String -> String
rpad n str = str ++ replicate (n - length str) ' '

--- swap the components of a tuple
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

--- zip three lists
zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 xs ys zs = case xs of
  []   -> []
  a:as -> case ys of
            []   -> []
            b:bs -> case zs of
                      []   -> []
                      c:cs -> (a, b, c) : zip3 as bs cs

--- Pretty print finite map
ppFM :: ((a, b) -> Doc) -> FM a b -> Doc
ppFM ppEntry fm = listSpaced $ map ppEntry $ fmToList fm

--- Perform an IO action in the given directory and return to the previous
--- directory afterwards
inDirectory :: String -> IO a -> IO a
inDirectory dir action = do
  prevDir <- getCurrentDirectory
  setCurrentDirectory dir
  r <- action
  setCurrentDirectory prevDir
  return r

-- -----------------------------------------------------------------------------
-- Tuple utilities
-- -----------------------------------------------------------------------------

-- Apply given function to first component of a tuple
mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

-- Apply given function to second component of a tuple
mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x, y) = (x, f y)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z

trTpl3 :: (a -> b) -> (c -> d) -> (e -> f) -> (a, c, e) -> (b, d, f)
trTpl3 f g h (x, y, z) = (f x, g y, h z)

-- TODO: Remove when there is a functor instance for Maybe
instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)
