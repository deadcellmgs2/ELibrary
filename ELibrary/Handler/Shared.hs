module Handler.Shared where

import Import

getSharedR :: Handler Html
getSharedR =do
            mid <- maybeAuthId
            documents <- runDB
               $   selectList [DocumentShared ==. True] [Desc DocumentId]
            defaultLayout $ do
              $(widgetFile "document/shared")

