{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Stack.New (createNewStackProject, StackProjectTemplate (..), NewStackOpts (..)) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as T

createNewStackProject :: (MonadIO m, MonadLogger m)
                      => NewStackOpts
                      -> m ()
createNewStackProject (NewStackOpts {..}) = do
  let templateName = maybe "default" id nsoTemplateName
  template <- loadTemplate templateName

  $logInfo $ "Creating project " <> sptName template

  return ()

defaultTemplate =
  let sptName = "default"
      sptHamletFilePaths = ["src/Main.hs"]
      sptOtherFilePaths = []
      sptCommandsToRun = [ "" ]
  in StackProjectTemplate {..}

loadTemplate :: (MonadIO m, MonadLogger m)
             => Text
             -> m StackProjectTemplate
loadTemplate name = do
  $logInfo $ "Can't actually load template lol"
  return $ defaultTemplate

data NewStackOpts = NewStackOpts { nsoProjectName  :: Text
                                 , nsoTemplateName :: Maybe Text }
                    deriving (Show)

data StackProjectTemplate = StackProjectTemplate
                            { sptName            :: Text
                            , sptHamletFilePaths :: [Text]
                            , sptOtherFilePaths  :: [Text]
                            , sptCommandsToRun   :: [Text]
                            } deriving (Show)
