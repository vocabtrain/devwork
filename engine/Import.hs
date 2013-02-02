module Import
    ( module Prelude
    , module Yesod
    , module Foundation
    , module Settings.StaticFiles
    , module Settings.Development
    , module Data.Monoid
    , module Control.Applicative
	, module Generated
    , Text
	, fromRightText
#if __GLASGOW_HASKELL__ < 704
    , (<>)
#endif
    ) where

import Prelude hiding (writeFile, readFile, head, tail, init, last)
import Yesod   hiding (Route(..))
import Foundation
#if __GLASGOW_HASKELL__ < 704
import Data.Monoid (Monoid (mappend, mempty, mconcat))
#else
import Data.Monoid (Monoid (mappend, mempty, mconcat), (<>))
#endif
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (Text)
import Settings.StaticFiles
import Settings.Development
import Generated

#if __GLASGOW_HASKELL__ < 704
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

	
fromRightText :: Either a Text -> Text
fromRightText (Right c) = c
fromRightText _ = ""

{-cardTypeField :: Field sub master CardTypePrimary
cardTypeField = selectFieldList cardTypePrimaryList
		where
			cardTypePrimaryList :: [(Text, CardTypePrimary)]
			cardTypePrimaryList = map (pack . show &&& id) $ [minBound..maxBound] 
-}
