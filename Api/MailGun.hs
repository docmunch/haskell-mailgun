module Api.MailGun where

-- use minimalistic imports rather than classy-prelude
-- so that this can be eastily factored into a library put on Hackage
import Prelude (IO, ($), String, map, return)
import Data.Text (Text, intercalate, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Monoid ((<>))
import Network.HTTP.Conduit
import Network.Mail.Mime (Address(..), Mail(..), renderMail')
import Network.HTTP.Client.MultipartFormData
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Maybe

addressToText :: Address -> Text
addressToText address =
    maybe "" (<> " ") (addressName address) <> "<" <> addressEmail address <> ">"

sendMailGun :: String        -- ^ domain
            -> BS.ByteString -- ^ username, should be "key"
            -> BS.ByteString -- ^ password
            -> Manager       -- ^ re-use an existing http manager
            -> Mail
            -> IO (Response LBS.ByteString)
sendMailGun domain user pass httpMan email = do
    bs <- renderMail' email
    request <- parseUrl $ "https://api.mailgun.net/v2/" <> domain <> "/messages.mime"
    preq <- postRequest (LBS.toStrict bs) request
    httpLbs (auth preq) httpMan
  where
    to = encodeUtf8 $ intercalate "," $ map addressToText $ mailTo email
    auth = applyBasicAuth user pass
    postRequest message = formDataBody
      [ partBS "to" to
      , partFileBS "message" message
      ]

-- | similar to 'partFile', but useful if you have the file contents already in memory
-- Normally you would use partBS, but an API may require the parameter to be a file
partFileBS :: Text -> BS.ByteString -> Part
partFileBS n fileContents = partFileRequestBodyM n (unpack n) $
        return $ RequestBodyBS fileContents
