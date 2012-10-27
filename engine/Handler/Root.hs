{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Root where

import Import

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
        $(widgetFile "root/root")
