module CompatIO where
import Prelude hiding (Monad(..))
import qualified Prelude as P

(>>=) :: IO a -> (a -> IO b) -> IO b
(>>=) = (P.>>=)

(>>) :: IO a -> IO b -> IO b
(>>) = (P.>>)

return :: a -> IO a
return = P.return