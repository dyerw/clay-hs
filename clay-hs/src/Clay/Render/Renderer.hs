module Clay.Render.Renderer where

import Clay.Color
import Control.Monad.IO.Class (MonadIO)

-- class (MonadIO m) => Renderer r m f i where
--   renderRect :: r -> Color -> Rect -> m ()
--   renderText :: r -> f -> Pos -> m ()
--   renderImage :: r -> i -> Rect -> m ()
--   renderCustom :: r -> c -> Rect -> m ()
