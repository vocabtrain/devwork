module Model where

import Prelude
import Yesod
import Data.Text (Text, pack)
import Database.Persist.Quasi
import Data.Time (UTCTime, getCurrentTime)
import Generated

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

--instance ToMessage VocabBookId where
--    toMessage i = Data.Text.pack $ show i
instance ToMessage Int where
    toMessage i = Data.Text.pack $ show i
