--- ----------------------------------------------------------------------------
--- This module provides some utility operations.
---
--- @author  Björn Peemöller, Jan Tikovsky
--- @version June 2017
--- ----------------------------------------------------------------------------
module Utils where

-- some useful combinators for monads
infixl 4 <$>, <*>

-- TODO: Remove when there are corresponding functions in the libraries
sequence :: Monad m => [m a] -> m [a]
sequence = foldr (\m n -> m >>= \x -> n >>= \xs -> return (x:xs)) (return [])

sequence_ :: Monad m => [m a] -> m ()
sequence_ = foldr (>>) (return ())

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f

mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f = sequence_ . map f

foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM _ z []     = return z
foldM f z (x:xs) = f z x >>= \z' -> foldM f z' xs

zipWithM_ :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ f xs ys = sequence_ $ zipWith f xs ys

whenM :: Monad m => Bool -> m () -> m ()
whenM p a = if p then a else return ()

unlessM :: Monad m => Bool -> m () -> m ()
unlessM p = whenM (not p)

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

-- TODO: Remove when there is a functor instance for Maybe
instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)
