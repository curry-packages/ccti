data Nat = IHi
         | O Nat
         | I Nat

add :: Nat -> Nat -> Nat
add IHi   y     = succ y
add (O x) IHi   = I x
add (O x) (O y) = O (add x y)
add (O x) (I y) = I (add x y)
add (I x) IHi   = O (succ x)
add (I x) (O y) = I (add x y)
add (I x) (I y) = O (add (succ x) y)

succ :: Nat -> Nat
succ IHi   = O IHi
succ (O x) = I x
succ (I x) = O (succ x)

main = add IHi (I (O IHi))
