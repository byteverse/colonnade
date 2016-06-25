module Siphon.Encoding where

import Siphon.Types
import Colonnade.Types
import Pipes (Pipe,yield)
import qualified Pipes.Prelude as Pipes
import qualified Colonnade.Encoding as Encoding

row :: Siphon c1 c2 
    -> Encoding f c1 a 
    -> a 
    -> c2
row (Siphon escape intercalate) e =
  intercalate . Encoding.runRow escape e

header :: Siphon c1 c2 
       -> Encoding Headed c1 a 
       -> c2
header (Siphon escape intercalate) e =
  intercalate (Encoding.runHeader escape e)

pipe :: Monad m => Siphon c1 c2 -> Encoding f c1 a -> Pipe a c2 m x
pipe siphon encoding = Pipes.map (row siphon encoding)

pipeWithHeader :: Monad m => Siphon c1 c2 -> Encoding Headed c1 a -> Pipe a c2 m x
pipeWithHeader siphon encoding = do
  yield (header siphon encoding)
  pipe siphon encoding

