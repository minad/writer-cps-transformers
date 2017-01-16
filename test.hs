--import Control.Monad.Trans.Writer.CPS
--import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.Writer.Stricter
import Control.Monad
import Data.Monoid

loop 0 = return ()
loop n = do
  tell $ Sum (1 :: Int)
  loop $ n - 1

main :: IO ()
--main = (print =<<) $ runWriterT $ replicateM_ 1000000000 $ tell $ Sum (1 :: Int)
main = runWriterT (loop 1000000000) >>= print
