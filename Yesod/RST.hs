-- | This module provides the functionallity to use
--   pandocs reader for reStructuredText (RST) in Yesod.
--   The code bases heavily on yesod-markdown from
--   Patrick Brisbin, which does the same thing for Markdown.

module Yesod.RST
  ( RST(..)
  -- * Wrappers
  , rstToHtml
  , rstToHtmlTrusted
  , rstFromFile
  -- * Conversions
  , parseRST
  , writePandoc
  , writePandocTrusted
  -- * Option sets
  , yesodDefaultWriterOptions
  , yesodDefaultParserState
  -- * Form helper
  , rstField
  )
  where

import Yesod.Form            (ToField(..), areq, aopt)
import Yesod.Core            (RenderMessage, SomeMessage(..))
import Yesod.Form.Types
import Yesod.Widget          (addHamlet)
import Text.Hamlet           (hamlet, Html)
import Database.Persist      (PersistField)

import Text.Blaze            (preEscapedString, preEscapedText)
import Text.Pandoc
import Text.HTML.SanitizeXSS (sanitizeBalance)

import Data.Monoid           (Monoid)
import Data.String           (IsString)
import System.Directory      (doesFileExist)

import Data.Text             (Text, pack, unpack)


newtype RST = RST String
   deriving (Eq, Ord, Show, Read, PersistField, IsString, Monoid)

instance ToField RST master where
   toField = areq rstField

instance ToField (Maybe RST) master where
   toField = aopt rstField

rstField :: RenderMessage master FormMessage => Field sub master RST
rstField = Field
    { fieldParse = blank $ Right . RST . unlines . lines' . unpack
    , fieldView = \theId name attrs val _isReq -> addHamlet
        [hamlet|
<textarea id="#{theId}" name="#{name}" *{attrs}>#{either id unRST val}
|]
    }
        where
        unRST :: RST -> Text
        unRST (RST s) = pack s

        lines' :: String  -> [String]
        lines' = map (filter (/= '\r')) . lines

blank :: (Monad m, RenderMessage master FormMessage)
      => (Text -> Either FormMessage a) -> [Text] -> m (Either (SomeMessage master) (Maybe a))
blank _ [] = return $ Right Nothing
blank _ ("":_) = return $ Right Nothing
blank f (x:_) = return $ either (Left . SomeMessage) (Right . Just) $ f x

-- | Converts RST to sanitizied Html
rstToHtml :: RST -> Html
rstToHtml = writePandoc yesodDefaultWriterOptions
          . parseRST yesodDefaultParserState

-- | Converts RST to unsanitizied Html
rstToHtmlTrusted :: RST -> Html
rstToHtmlTrusted = writePandocTrusted yesodDefaultWriterOptions
                 . parseRST yesodDefaultParserState

-- | Reads RST in from the specified file; returns the empty string
--   if the file does not exist
rstFromFile :: FilePath -> IO RST
rstFromFile f = do
    exists <- doesFileExist f
    content <- do
        if exists
           then readFile f
           else return ""
    return $ RST content

-- | Converts the intermediate Pandoc type to Html. Sanitizes HTML.
writePandoc :: WriterOptions -> Pandoc -> Html
writePandoc wo = preEscapedText . sanitizeBalance . pack . writeHtmlString wo

-- | Skips the sanitization and its required conversion to Text
writePandocTrusted :: WriterOptions -> Pandoc -> Html
writePandocTrusted wo = preEscapedString . writeHtmlString wo

-- | Parses Markdown into the intermediate Pandoc type
parseRST :: ParserState -> RST -> Pandoc
parseRST ro (RST m) = readRST ro m

-- | Pandoc defaults, plus Html5, minus WrapText
yesodDefaultWriterOptions :: WriterOptions
yesodDefaultWriterOptions = defaultWriterOptions
  { writerHtml5    = True
  , writerWrapText = False
  }

-- | Pandoc defaults, plus Smart, plus ParseRaw
yesodDefaultParserState :: ParserState
yesodDefaultParserState = defaultParserState
    { stateSmart    = True
    , stateParseRaw = True
    }
