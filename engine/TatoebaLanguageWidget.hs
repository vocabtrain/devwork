module TatoebaLanguageWidget where

import Import
import qualified Data.Text as Text
import ToAppMessage 
import PostGenerated () 

tatoebaLanguageWidget :: TatoebaLanguage -> GWidget App App()
tatoebaLanguageWidget lang = do
	msgShow <- lift $ getMessageRender
	toWidget $ [hamlet|<i .flag-#{Text.pack $ show $ lang} title=#{msgShow $ toAppMessage lang} data-placement="bottom" rel="tooltip"> |]

