module TatoebaLanguageWidget where

import Import
import qualified Data.Text as Text
import ToAppMessage 
import PostGenerated () 

tatoebaLanguageWidget :: TatoebaLanguage -> Widget
tatoebaLanguageWidget lang = do
	msgShow <- getMessageRender
	toWidget $ [hamlet|<i .flag-#{Text.pack $ show $ lang} title=#{msgShow $ toAppMessage lang} data-placement="bottom" rel="tooltip"> |]

