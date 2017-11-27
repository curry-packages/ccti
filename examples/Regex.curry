data Regex a = Lit a
             | Conc (Regex a) (Regex a)
             | Alt (Regex a) (Regex a)
             | Star (Regex a)

sem :: Regex a -> [a]
sem (Lit    c) = [c]
sem (Conc a b) = sem a ++ sem b
sem (Alt  a b) = sem a ? sem b
sem (Star   a) = [] ? sem (Conc a (Star a))

data Alpha = A | B

main = sem (Lit B) -- testSem (Conc (Star (Lit B)) (Lit A))

testSem :: Regex a -> a
testSem r = head (sem r)
