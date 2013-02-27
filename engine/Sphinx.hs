module Sphinx where

import Prelude (map)
import qualified Text.Search.Sphinx as Sphinx
import qualified Text.Search.Sphinx.Types as SphinxT
import Import
import GHC.Int

querySphinx :: Text -> Text -> IO [Int64]
querySphinx lang queryStr = do
	result <- liftIO $ Sphinx.query config lang queryStr
	case result of
		SphinxT.Ok res -> return $ map SphinxT.documentId $ SphinxT.matches res
		_ -> return []

		where config = Sphinx.defaultConfig 
			{ Sphinx.port = 9312
			, Sphinx.sort = SphinxT.Relevance
			, Sphinx.mode = SphinxT.Any
			}

