module Siphon.Encoding where

import Siphon.Types
import Colonnade.Types
import Pipes (Pipe,yield)
import qualified Pipes.Prelude as Pipes
import qualified Colonnade.Encoding as Encoding

row :: Siphon c
    -> Encoding f c a
    -> a
    -> c
row (Siphon escape intercalate _ _) e =
  intercalate . Encoding.runRow escape e

header :: Siphon c
       -> Encoding Headed c a
       -> c
header (Siphon escape intercalate _ _) e =
  intercalate (Encoding.runHeader escape e)

pipe :: Monad m
  => Siphon c
  -> Encoding f c a
  -> Pipe a c m x
pipe siphon encoding = Pipes.map (row siphon encoding)

headedPipe :: Monad m
  => Siphon c
  -> Encoding Headed c a
  -> Pipe a c m x
headedPipe siphon encoding = do
  yield (header siphon encoding)
  pipe siphon encoding

