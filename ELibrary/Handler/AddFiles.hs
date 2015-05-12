module Handler.AddFiles where

import Import
import Yesod.Static
import Settings.StaticFiles
import Yesod.Form.Bootstrap3
import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding
import System.Random
import System.FilePath as FPT
import Database.Persist.Sql
import Data.Text
import System.Directory (removeFile, doesFileExist)

uploadDirectory :: FPT.FilePath
uploadDirectory = "static"

reluploadDirectory :: FPT.FilePath
reluploadDirectory = "../static"

uploadForm :: Form (FileInfo, Maybe Text)
uploadForm = renderBootstrap $ (,)
             <$> fileAFormReq (bfs MsgFilerq)
             <*> aopt textField (bfs MsgFileContent) Nothing

getAddFilesR :: DocumentId -> Handler Html
getAddFilesR id = do
 (widget, enctype) <- generateFormPost uploadForm
 defaultLayout $ do
     setTitle "Choose a File"
     $(widgetFile "document/addFile")
     
postAddFilesR :: DocumentId -> Handler RepHtml
postAddFilesR id = do
  ((result,widget),enctype) <- runFormPost uploadForm
  case result of
   FormSuccess (fi,info) -> do
                            setMessage "Image Saved"
                            writeToServer fi id
                            redirect HomeR
   _ -> do
     setMessage "Image saved"
     redirect HomeR
  
writeToServer :: FileInfo -> DocumentId -> Handler FPT.FilePath
writeToServer file id = do
  let filename = Import.unpack $ fileName file
      path = filePath filename
  liftIO $ fileMove file path
  return filename

filePath :: String -> FPT.FilePath
filePath f = uploadDirectory FPT.</> f

relfilePath :: String -> FPT.FilePath
relfilePath f = reluploadDirectory FPT.</> f


getDocumentR :: DocumentId -> Handler TypedContent
getDocumentR bpid = do
  document <- runDB $ get404 bpid
  selectRep $ do
    provideRep $ defaultLayout $ do
      $(widgetFile "document/details")

getDeleteR :: DocumentId -> Handler ()
getDeleteR bid = do
  document <- runDB $ get404 bid
  let filename = Import.unpack $ documentName document
      path = filePath filename
  liftIO $ removeFile path
  stillExists <- liftIO $ doesFileExist path

  case (not stillExists) of
   False -> redirect HomeR
   True -> do
     runDB $ delete bid
     redirect HomeR
  
