module Import
    ( module Prelude
    , module Yesod
    , module Foundation
    , module Settings.StaticFiles
    , module Settings.Development
    , module Data.Monoid
    , module Control.Applicative
	, module Generated
	, staticServer
    , Text
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
import qualified Data.Text
import Settings.StaticFiles
import Settings.Development
import Generated

#if __GLASGOW_HASKELL__ < 704
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif


staticServer :: Text -> Text
staticServer str = Data.Text.concat [ "http://web403.webbox555.server-home.org/drake/", str ]

