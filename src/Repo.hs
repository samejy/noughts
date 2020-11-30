{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Repo where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Control.Monad.IO.Unlift
import           Data.Text
import           Control.Monad.Reader
import           Control.Monad.Logger
import           Conduit

share [mkPersist sqlSettings, mkSave "entityDefs", mkMigrate "migrateAll"] [persistLowerCase|
DBMove
  before String
  after String
  score Int
  deriving Show
|]

-- https://www.yesodweb.com/book/persistent


-- putMove :: Text -> Text -> Int -> SqlBackend something something
-- putMove b b' s = insert $ DBMove b b' s
-- -- and updates?

-- getMoves :: Text -> SqlBackend something [Entity SqlPersistM DBMove]
-- getMoves b = selectList [DBMoveBefore ==. b] []

runSqlite' :: (MonadUnliftIO m) => Text -> ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runSqlite' = runSqlite

test :: IO ()
test = runSqlite' "db.sqlite" $ do
    runMigration migrateAll -- $ migrate entityDefs $ entityDef (Nothing :: Maybe Person)
    insertId <- insert $ DBMove "before" "after" 0
    inserted <- get insertId
    liftIO $ print inserted
