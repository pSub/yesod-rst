-- | This module provides the functionallity to use
--   pandocs reader for reStructuredText (RST) in Yesod.
--   The code bases heavily on yesod-markdown from
--   Patrick Brisbin, which does the same thing for Markdown.

module Yesod.RST
  ( RST(..)
  -- * Wrappers
  , rstToHtml
  , rstToHtmlTrusted
  , rstToHtmlWithExtensions
  , rstToHtmlWith
  , rstFromFile
  -- * Conversions
  , parseRST
  , writePandoc
  , writePandocTrusted
  -- * Option sets
  , yesodDefaultWriterOptions
  , yesodDefaultReaderOptions
  , yesodDefaultExtensions
  -- * Form helper
  , rstField
  ) where

import Control.Monad ((<=<))

import Yesod.Form            (areq, aopt)
import Yesod.Core            (HandlerSite, RenderMessage)
import Yesod.Core.Widget     (toWidget)
import Yesod.Form.Types
import Yesod.Form.Functions  (parseHelper)
import Text.Hamlet           (hamlet, Html)
import Database.Persist      (PersistField, SqlType(SqlString))
import Database.Persist.Sql  (PersistFieldSql(..))


import Text.Blaze            (ToMarkup(toMarkup))
import Text.Blaze.Html       (preEscapedToMarkup)
import Text.Pandoc    hiding (handleError)
import Text.HTML.SanitizeXSS (sanitizeBalance)

import Data.Monoid           (Monoid)
import Data.String           (IsString)
import System.Directory      (doesFileExist)

import Data.Text             (Text, pack, unpack)
import Data.Text.Encoding    (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)


import qualified Data.ByteString as B


newtype RST = RST { unRST :: Text }
   deriving (Eq, Ord, Show, Read, PersistField, IsString, Monoid, Semigroup)

instance PersistFieldSql RST where
    sqlType _ = SqlString

instance ToMarkup RST where
    -- | Sanitized by default
    toMarkup = handleError . rstToHtml

rstField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m RST
rstField = Field
    { fieldParse = parseHelper $ Right . RST
    , fieldView = \theId name attrs val _isReq -> toWidget
        [hamlet|
<textarea id="#{theId}" name="#{name}" *{attrs}>#{either id unRST val}
|]
   , fieldEnctype = UrlEncoded
    }

-- | Process RST using our options and sanitization
rstToHtml :: RST -> Either PandocError Html
rstToHtml = rstToHtmlWith
    yesodDefaultReaderOptions
    yesodDefaultWriterOptions

-- | No HTML sanitization
--
-- **NOTE**: Use only with /fully-trusted/ input.
--
rstToHtmlTrusted :: RST -> Either PandocError Html
rstToHtmlTrusted = rstToHtmlWith' id
    yesodDefaultReaderOptions
    yesodDefaultWriterOptions

-- | Process RST with given extensions
--
-- Uses our options, and overrides extensions only.
--
rstToHtmlWithExtensions
    :: Extensions
    -> RST
    -> Either PandocError Html
rstToHtmlWithExtensions exts = rstToHtmlWith
    yesodDefaultReaderOptions { readerExtensions = exts }
    yesodDefaultWriterOptions { writerExtensions = exts }

-- | Fully controllable RST processing
rstToHtmlWith
    :: ReaderOptions
    -> WriterOptions
    -> RST
    -> Either PandocError Html
rstToHtmlWith = rstToHtmlWith' sanitizeBalance

-- | Internal function, the only way to skip sanitization
rstToHtmlWith'
    :: (Text -> Text)
    -> ReaderOptions
    -> WriterOptions
    -> RST
    -> Either PandocError Html
rstToHtmlWith' sanitize ropts wopts =
    writePandocWith sanitize wopts <=< parseRST ropts


-- | Returns the empty string if the file does not exist
rstFromFile :: FilePath -> IO RST
rstFromFile f = do
    exists <- doesFileExist f
    RST <$> if exists
        then readFileUtf8 f
        else return ""

  where
    readFileUtf8 :: FilePath -> IO Text
    readFileUtf8 fp = decodeUtf8With lenientDecode <$> B.readFile fp

writePandoc :: WriterOptions -> Pandoc -> Either PandocError Html
writePandoc = writePandocWith sanitizeBalance
{-# DEPRECATED writePandoc "Don't use this directly" #-}

writePandocTrusted :: WriterOptions -> Pandoc -> Either PandocError Html
writePandocTrusted = writePandocWith id
{-# DEPRECATED writePandocTrusted "Don't use this directly" #-}

writePandocWith
    :: (Text -> Text)
    -> WriterOptions
    -> Pandoc
    -> Either PandocError Html
writePandocWith f wo
    = (preEscapedToMarkup . f <$>)
    . runPure
    . writeHtml5String wo

-- | Parses RST into the intermediate Pandoc type
parseRST :: ReaderOptions -> RST -> Either PandocError Pandoc
parseRST ro = runPure . readRST ro . unRST
{-# DEPRECATED parseRST "Don't use this directly" #-}

-- | Defaults minus WrapText, plus our extensions
yesodDefaultWriterOptions :: WriterOptions
yesodDefaultWriterOptions = def
    { writerWrapText = WrapNone
    , writerExtensions = extensionsFromList yesodDefaultExtensions
    }

-- | Defaults plus our extensions, see @'yesodDefaultExtensions'@
yesodDefaultReaderOptions :: ReaderOptions
yesodDefaultReaderOptions = def
    { readerExtensions = extensionsFromList yesodDefaultExtensions
    }

-- | @raw_html@ and @auto_identifiers@
yesodDefaultExtensions :: [Extension]
yesodDefaultExtensions =
    [ Ext_raw_html
    , Ext_auto_identifiers
    ]

-- | Unsafely handle a @'PandocError'@
--
-- This is analagous to pandoc-1 behavior, and is required in a pure context
-- such as the @'ToMarkup'@ instance.
--
handleError :: Either PandocError a -> a
handleError = either (error . show) id
