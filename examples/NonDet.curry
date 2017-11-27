data Peano = Z | S Peano

insertND :: a -> [a] -> [a]
insertND x []     = [x]
insertND x (y:ys) = x : y : ys
insertND x (y:ys) = y : insertND x ys

perm :: [a] -> [a]
perm [] = []
perm (x:xs) = insertND x (perm xs)

main = perm [False]
