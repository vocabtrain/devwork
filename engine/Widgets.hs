module Widgets where

import Import

zoomImageWidget :: Text -> Route App -> WidgetT App IO ()
zoomImageWidget alt route = do
	toWidget $ [hamlet|<img .zoomTarget data-targetsize="1.0" data-closeclick="true" alt=#{alt} src=@{route}> |]

