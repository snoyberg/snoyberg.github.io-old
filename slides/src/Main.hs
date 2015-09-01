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
import Text.Hamlet
import Text.Hamlet.RT
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

readHamletFile :: FilePath -> IO HamletRT
readHamletFile fp = do
    bs <- readFile fp
    parseHamletRT defaultHamletSettings $ unpack $ asText $ decodeUtf8 bs

renderHamlet :: MonadThrow m => HamletRT -> HamletMap Void -> m Html
renderHamlet rt m = renderHamletRT rt m $ \x _ -> absurd x

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
    template <- readHamletFile configTemplate
    forM_ configDecks $ \Deck {..} -> do
        slides <- forM deckSlides $ \slide -> do
            rt <- readHamletFile $ configSnippets </> unpack slide <.> "hamlet"
            renderHamlet rt []
        let m =
                [ (["autogenWarning"], HDHtml "NOTE: This file was autogenerating, do not edit!")
                , (["title"], HDHtml $ toHtml deckTitle)
                , (["author"], HDHtml $ toHtml deckAuthor)
                , (["presenter"], HDHtml $ toHtml deckPresenter)
                , (["event"], HDHtml $ toHtml deckEvent)
                , (["slides"], HDList $ map (\s -> [([], HDHtml s)]) $ unpack slides)
                ]
        html <- renderHamlet template m
        let dest = configOutputDir </> unpack deckSlug <.> "html"
        putStrLn $ "Generating " ++ deckSlug
        writeFile dest $ renderHtml html
