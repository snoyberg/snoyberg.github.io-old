{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Main where

import Text.Blaze.Html (toHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import ClassyPrelude
import Data.Aeson
import Data.Yaml
import Text.Hamlet.Runtime
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Data.Void

data Deck = Deck
    { deckAuthor :: !Text
    , deckTitle :: !Text
    , deckPresenter:: !Text
    , deckEvent :: !Text
    , deckSlug :: !Text
    , deckSlides :: !(Vector Text)
    }
instance FromJSON Deck where
    parseJSON = withObject "Deck" $ \o -> Deck
        <$> o .: "author"
        <*> o .: "title"
        <*> o .: "presenter"
        <*> o .: "event"
        <*> o .: "slug"
        <*> o .: "slides"

data Config filepath = Config
    { configTemplate :: !filepath
    , configSnippets :: !filepath
    , configOutputDir :: !filepath
    , configDecks :: !(Vector Deck)
    }
    deriving Functor
instance (filepath ~ Text) => FromJSON (Config filepath) where
    parseJSON = withObject "Config" $ \o -> Config
        <$> o .: "template"
        <*> o .: "snippets-dir"
        <*> o .: "output-dir"
        <*> o .: "decks"

main :: IO ()
main = do
    args <- getArgs
    configFP' <-
        case args of
            [] -> return "decks.yaml"
            [x] -> return x
            _ -> error "Usage: slides [decks.yaml]"
    configFP <- canonicalizePath $ unpack configFP'
    config0 <- decodeFileEither configFP >>= either throwM return
    let Config {..} = fmap ((takeDirectory configFP </>) . unpack) config0
    createDirectoryIfMissing True configOutputDir
    template <- readHamletTemplateFile defaultHamletSettings configTemplate
    forM_ configDecks $ \Deck {..} -> do
        slides <- forM deckSlides $ \slide -> do
            rt <- readHamletTemplateFile defaultHamletSettings
                $ configSnippets </> unpack slide <.> "hamlet"
            renderHamletTemplate rt mempty
        let m = mapFromList
                [ ("autogenWarning", "NOTE: This file was autogenerating, do not edit!")
                , ("title", toHamletData deckTitle)
                , ("author", toHamletData deckAuthor)
                , ("presenter", toHamletData deckPresenter)
                , ("event", toHamletData deckEvent)
                , ("slides", toHamletData $ map toHamletData $ unpack slides)
                ]
        html <- renderHamletTemplate template m
        let dest = configOutputDir </> unpack deckSlug <.> "html"
        putStrLn $ "Generating " ++ deckSlug
        writeFile dest $ renderHtml html
