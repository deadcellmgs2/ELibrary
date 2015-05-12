
module Handler.NewDocument where

import Import
import Yesod.Form.Bootstrap3
import Yesod.Text.Markdown (markdownField)
import Yesod.Static
import Settings.StaticFiles
import Data.Text
documentPostForm:: UserId  -> Maybe Document -> AForm Handler Document
documentPostForm uid mDocument = Document
                                <$> pure uid
                                <*> areq textField (bfs MsgName) ( documentName <$> mDocument)
                                <*> areq markdownField (bfs MsgDescripcion) (documentDescripcion <$> mDocument)
                                <*> areq (selectFieldList rates) (bfs MsgRate) (documentRate <$> mDocument)
                                <*> areq boolField (bfs MsgShared) (documentShared <$> mDocument)
 where
  rates:: [(Text, Int)]
  rates = [("one",1),("two", 2),("three", 3),("four", 4),("five",5)]


  
getNewDocumentR :: Handler Html
getNewDocumentR = do
  uid <- requireAuthId
  (widget,encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ documentPostForm uid Nothing
  defaultLayout $ do
    $(widgetFile "document/new")

postNewDocumentR :: Handler Html
postNewDocumentR = do
  uid <- requireAuthId
  ((result,widget),encoding) <- runFormPost $
                                renderBootstrap3 BootstrapBasicForm $ documentPostForm uid Nothing
  case result of
    FormSuccess document -> do
                            bid <- runDB $ insert document
                            redirect (AddFilesR bid)
    _ -> defaultLayout $ do
      $(widgetFile "document/new")

  
      
getDocumentUpdateR :: DocumentId -> Handler Html
getDocumentUpdateR bid = do
  uid <- requireAuthId
  document <- runDB $ get404 bid
  (widget,encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ documentPostForm uid (Just document)
  defaultLayout $ do
    $(widgetFile "document/edit")

postDocumentUpdateR :: DocumentId -> Handler Html
postDocumentUpdateR bid = do
  uid <- requireAuthId
  document <- runDB $ get404 bid
  ((result,widget),encoding) <- runFormPost $
                                renderBootstrap3 BootstrapBasicForm $ documentPostForm uid (Just document)
  case result of
    FormSuccess document -> do
                            runDB $ Import.replace bid document
                            redirect (AddFilesR bid)
    _ -> defaultLayout $ do
      $(widgetFile "document/edit")
