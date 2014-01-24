module BibtexWidget where

import Import
import qualified Text.BibTeX.Parse as Parse
import qualified Text.BibTeX.Entry as Entry
import qualified Text.ParserCombinators.Parsec as Parsec

import qualified System.IO as IO
import qualified Data.Text as Text
import Yesod.Core (renderRoute)
import Yesod.Static (StaticRoute)
import Text.Regex (subRegex,mkRegex,Regex)

import Text.Blaze (toMarkup, Markup)
import Text.Blaze.Html.Renderer.String 
{-
Data.Text 
Data.Text.Encoding 
Text.Blaze.Renderer.Utf8 
Data.ByteString.Lazy 
Text.Blaze
-}

texSzlig :: Regex
texSzlig = mkRegex "{\\\\ss}"
texUmlaute :: Regex
-- texUmlaute = mkRegex "{\\\\\"([A-Za-z])}"
texUmlaute = mkRegex "{\\\\&quot;([A-Za-z])}"


texEscapeToHtml :: String -> Markup
texEscapeToHtml s = preEscapedToMarkup $ filter (\a -> a /= '{' && a /= '}') $ subRegex texUmlaute ss "&\\1uml;" 
	where 
		str = renderHtml $ toMarkup s
		ss = subRegex texSzlig str "&szlig;"


fetchBibtex :: FilePath -> IO [Entry.T]
fetchBibtex filepath = IO.withFile ("/home/niki/devwork/devwork/engine/" ++ filepath) IO.ReadMode $ \h -> do
		bib <- IO.hGetContents h 
		case Parsec.parse (Parsec.skipMany Parsec.space >> Parse.file) "" bib of
			 Left errMsg -> do
				  IO.hPutStrLn IO.stderr (show errMsg)  
				  return []
			 Right entries -> return entries

bibtexWidget :: StaticRoute -> Widget
bibtexWidget r = do
	entries <- liftIO $ fetchBibtex $ Text.unpack $ Text.intercalate "/" $ fst $ renderRoute $ StaticR r
	toWidget $(whamletFile "templates/widgets/bibtex.hamlet")
