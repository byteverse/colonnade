module Reflex.Dom.Colonnade where

import Colonnade.Types
import Reflex.Dom.Widget.Basic

-- hmm...
-- data WithAttrs 

basic :: MonadWidget t m
      => Encoding Headed (m ()) Int
      -> m ()
basic (Encoding v) = do
  el "table" $ do
    el "thead" $ mapM_ (getHeaded . fst) v

