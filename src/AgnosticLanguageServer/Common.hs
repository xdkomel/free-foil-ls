module AgnosticLanguageServer.Common
  ( maybeToEither
  , firstJustL
  , lastJustR
  , inRange
  ) where

import Language.LSP.Protocol.Types (Position(..), Range(..))

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither l = maybe (Left l) Right

firstJustL :: [Maybe a] -> Maybe a
firstJustL (j@(Just _):_) = j
firstJustL (_:t) = firstJustL t
firstJustL [] = Nothing

lastJustR :: Maybe a -> Maybe a -> Maybe a
lastJustR = maybe id (const . Just)

inRange :: Position -> Range -> Bool
inRange (Position l c) (Range (Position x y) (Position x' y')) =
  let startsOK = (l /= x) || (c >= y)
      endsOK = (l /= x') || (c <= y')
  in l >= x && l <= x' && startsOK && endsOK
