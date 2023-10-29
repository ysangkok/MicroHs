module Class(main) where
import Primitives
import Prelude

class Eqq a where
  (===) :: a -> a -> Bool
  (/==) :: a -> a -> Bool
  x /== y = not (x === y)

instance Eqq Int where
  (===) = primIntEQ

instance Eqq Char where
  (===) = primCharEQ

instance forall a . Eqq a => Eqq [a] where
  []     === []      =  True
  (x:xs) === (y:ys)  =  x === y && xs === ys
  _      === _       =  False

class (Eqq a) => Ordd a where
  (<==) :: a -> a -> Bool

instance Ordd Int where
  (<==) = (<=)

instance forall a b . (Eqq a, Eqq b) => Eqq (a, b) where
  (a, b) === (a', b')  =  a === a' && b === b'

f :: forall a . Eqq a => a -> Bool
f x = x === x

g :: forall a . Ordd a => a -> Bool
g x = x /== x

h :: forall a b . (Eqq a, Eqq b) => a -> b -> Bool
h a b = a === a && b === b

main :: IO ()
main = do
  putStrLn $ showBool $ f 5
  putStrLn $ showBool $ g 5
  putStrLn $ showBool $ h 5 'a'
  putStrLn $ showBool $ f [88]
  putStrLn $ showBool $ f (1, 'a')