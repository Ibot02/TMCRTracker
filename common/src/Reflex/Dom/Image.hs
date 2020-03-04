{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.Dom.Image where

import Reflex.Dom (elAttr, elAttr', DomBuilder(), blank, (=:), Element(), EventResult(), DomBuilderSpace())
import Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.FileEmbed (bsToExp)
import Data.Text.Encoding
import Data.Monoid
import Data.Text (Text)
import Language.Haskell.TH.Syntax

data Image = Image {
            _imagePath :: FilePath,
            _imageContents :: ByteString
            } deriving (Eq, Ord)

instance Lift Image where
    lift (Image path contents) = [| Image path $(bsToExp contents) |]

image :: FilePath -> IO Image
image path = Image path <$> BS.readFile path

staticImage :: FilePath -> Q (TExp Image)
staticImage path = runIO (image path) >>= \x -> [|| x ||]

fileURI :: Text -> BS.ByteString -> Text
fileURI mimetype contents = "data:" <> mimetype <> ";base64," <> (decodeUtf8 $ Base64.encode $ contents) 

toURI :: Image -> Text
toURI img = fileURI "image/png" $ _imageContents img

displayImage :: (DomBuilder t m) => Image -> m ()
displayImage img = elAttr "img" ("src" =: toURI img) blank

displayImage' :: (DomBuilder t m) => Image -> m (Element EventResult (DomBuilderSpace m) t)
displayImage' img = fmap fst $ elAttr' "img" ("src" =: toURI img) blank
