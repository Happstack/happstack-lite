{-# LANGUAGE RecordWildCards #-}
module Happstack.Lite 
     ( -- * Core Types
       Request
     , Response
     , ServerPart
     -- * Starting the Server
     , ServerConfig(..)
     , defaultServerConfig
     , serve
     -- * Routing an Incoming Request
     , method
     , Method(..)
     , MatchMethod(..)
     , dir
     , path
     , FromReqURI(..)
     , nullDir
     , guardRq
     -- * Creating a Response
     , ToMessage(..)
     , toResponseBS
     -- * Setting the Response Code
     , ok
     , internalServerError
     , unauthorized
     , notFound
     , seeOther
     , setResponseCode
     -- * Looking up Request Parameters
     , lookBS
     , lookBSs
     , lookText
     , lookTexts
     , lookFile
     , ContentType(..)
     -- * Cookies
     , Cookie(..)
     , CookieLife(..)
     , mkCookie  
     , addCookies
     , expireCookie
     , lookCookieValue
     -- * HTTP Headers
     , addHeaderM
     , setHeaderM
     , getHeaderM
     -- * File Serving
     , Browsing(..)
     , serveDirectory
     , serveFile
     , asContentType
     -- * Other
     , msum
     ) where

import Control.Monad (MonadPlus, msum)
import qualified Data.ByteString as B
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import Happstack.Server (ContentType, Request, Response, ServerPart, FromReqURI, Method(..), MatchMethod, ToMessage(..), Cookie(..), CookieLife(..), Browsing, mkCookie, toResponseBS)
import Happstack.Server.SURI (ToSURI)
import qualified Happstack.Server as S

-- * Starting the server


-- | configuration to be used with 'serve' function
data ServerConfig = 
    ServerConfig { port      :: Int       -- ^ port to listen on
                 , ramQuota  :: Int64     -- ^ maximum amount of POST data (in bytes)
                 , diskQuota :: Int64     -- ^ maximum file upload size (in bytes)
                 , tmpDir    :: FilePath  -- ^ temporary directory for file uploads
                 }

-- | a reasonable default 'ServerConfig'
-- 
-- > ServerConfig { port      = 8000
-- >              , ramQuota  = 1 * 10^6
-- >              , diskQuota = 20 * 10^6
-- >              , tmpDir    = "/tmp/"
-- >              }
defaultServerConfig :: ServerConfig
defaultServerConfig =
    ServerConfig { port      = 8000
                 , ramQuota  = 1 * 10^6
                 , diskQuota = 20 * 10^6
                 , tmpDir    = "/tmp/"
                 }

serve :: Maybe ServerConfig -> ServerPart Response -> IO ()
serve mServerConf part = 
    let ServerConfig{..} = fromMaybe defaultServerConfig mServerConf
    in S.simpleHTTP (S.nullConf { S.port = port }) $
         do S.decodeBody (S.defaultBodyPolicy tmpDir diskQuota ramQuota (ramQuota `div` 10))
            part

-- * Routing on a URI path segment

-- | Pop a path element and run the supplied handler if it matches the
-- given string.
-- 
-- > handler :: ServerPart Response
-- > handler = dir "foo" $ dir "bar" $ subHandler
-- 
-- The path element can not contain \'/\'. See also 'dirs'.
dir :: String -> ServerPart a -> ServerPart a
dir = S.dir

-- | Pop a path element and parse it using the 'fromReqURI' in the
-- 'FromReqURI' class.
path :: (FromReqURI a) => (a -> ServerPart b) -> ServerPart b
path = S.path

-- | guard which only succeeds if there are no remaining path segments
--
-- Often used if you want to explicitly assign a route for '/'
-- 
nullDir :: ServerPart ()
nullDir = S.nullDir

-- | Guard using an arbitrary function on the 'Request'.
guardRq :: (Request -> Bool) -> ServerPart ()
guardRq = S.guardRq

-- * Routing on the HTTP Request method

-- | Guard against the request method
--
-- Example:
--
-- > handler :: ServerPart Response
-- > handler =
-- >     do method [GET, HEAD]
-- >        ...
method :: (MatchMethod method) => method -> ServerPart ()
method = S.methodOnly 

-- * Creating a Response

toResponse :: (ToMessage a) => a -> Response
toResponse = S.toResponse

-- * Response code

-- | Respond with @200 OK@.
-- 
-- > main = serve Nothing $ ok "Everything is OK"
ok :: a -> ServerPart a
ok = S.ok

-- | Respond with @204 No Content@
--
-- A @204 No Content@ response may not contain a message-body. If you try to supply one, it will be dutifully ignored.
--
-- > main = serve Nothing $ noContent "This will be ignored."
noContent :: a -> ServerPart a
noContent = S.noContent

-- | Respond with @500 Internal Server Error@.
--
-- > main = serve Nothing $ internalServerError "Sorry, there was an internal server error."
internalServerError :: a -> ServerPart a
internalServerError = S.internalServerError

-- | Responds with @502 Bad Gateway@.
--
-- > main = serve Nothing $ badGateway "Bad Gateway."
badGateway :: a -> ServerPart a
badGateway = S.badGateway

-- | Respond with @400 Bad Request@.
--
-- > main = serve Nothing $ badRequest "Bad Request."
badRequest :: a -> ServerPart a
badRequest = S.badRequest

-- | Respond with @401 Unauthorized@.
--
-- > main = serve Nothing $ unauthorized "You are not authorized."
unauthorized :: a -> ServerPart a
unauthorized = S.unauthorized

-- | Respond with @403 Forbidden@.
--
-- > main = serve Nothing $ forbidden "Sorry, it is forbidden."
forbidden :: a -> ServerPart a
forbidden = S.forbidden

-- | Respond with @404 Not Found@.
-- 
-- > main = serve Nothing $ notFound "What you are looking for has not been found."
notFound :: a -> ServerPart a
notFound = S.notFound

-- | Set an arbitrary return code in your response.
--
-- A filter for setting the response code. Generally you will use a
-- helper function like 'ok' or 'seeOther'.
-- 
-- > main = serve Nothing $ do setResponseCode 200
-- >                           return "Everything is OK"
-- 
setResponseCode :: Int -- ^ response code
                -> ServerPart ()
setResponseCode = S.setResponseCode

-- | Respond with @413 Request Entity Too Large@.
--
-- > main = serve Nothing $ requestEntityTooLarge "That's too big for me to handle."
requestEntityTooLarge :: a -> ServerPart a
requestEntityTooLarge = S.requestEntityTooLarge

-- | Respond with @303 See Other@.
--
-- > main = serve Nothing $ seeOther "http://example.org/" "What you are looking for is now at http://example.org/"
--
-- NOTE: The second argument of 'seeOther' is the message body which will sent to the browser. According to the HTTP 1.1 spec,
--
-- @the entity of the response SHOULD contain a short hypertext note with a hyperlink to the new URI(s).@
--
-- This is because pre-HTTP\/1.1 user agents do not support 303. However, in practice you can probably just use @\"\"@ as the second argument.
seeOther :: (ToSURI uri) => uri -> a -> ServerPart a
seeOther = S.seeOther

-- | Respond with @302 Found@.
-- 
-- You probably want 'seeOther'. This method is not in popular use anymore, and is generally treated like 303 by most user-agents anyway.
found :: (ToSURI uri) => uri -> a -> ServerPart a
found = S.found

-- | Respond with @301 Moved Permanently@.
--
-- > main = serve Nothing $ movedPermanently "http://example.org/" "What you are looking for is now at http://example.org/"
movedPermanently :: (ToSURI uri) => uri -> a -> ServerPart a
movedPermanently = S.movedPermanently

-- | Respond with @307 Temporary Redirect@.
--
-- > main = serve Nothing $ tempRedirect "http://example.org/" "What you are looking for is temporarily at http://example.org/"
tempRedirect :: (ToSURI uri) => uri -> a -> ServerPart a
tempRedirect = S.tempRedirect

-- * Request Parameters

-- | Gets the first matching named input parameter as a lazy 'ByteString'
--
-- Searches the QUERY_STRING followed by the Request body.
--
-- see also: 'lookBSs'
lookBS :: String -> ServerPart ByteString
lookBS = S.lookBS

-- | Gets all matches for the named input parameter as lazy 'ByteString's
--
-- Searches the QUERY_STRING followed by the Request body.
--
-- see also: 'lookBS'
lookBSs :: String -> ServerPart [ByteString]
lookBSs = S.lookBSs

-- | Gets the first matching named input parameter as a lazy 'Text'
--
-- Searches the QUERY_STRING followed by the Request body.
--
-- This function assumes the underlying octets are UTF-8 encoded.
--
-- see also: 'lookTexts'
lookText :: String -> ServerPart Text
lookText = S.lookText

-- | Gets all matches for the named input parameter as lazy 'Text's
--
-- Searches the QUERY_STRING followed by the Request body.
--
-- This function assumes the underlying octets are UTF-8 encoded.
--
-- see also: 'lookText'
lookTexts :: String -> ServerPart [Text]
lookTexts = S.lookTexts

-- | Gets the first matching named file
--
-- Files can only appear in the request body. Additionally, the form
-- must set enctype=\"multipart\/form-data\".
--
-- This function returns a tuple consisting of:
-- 
--  (1) The temporary location of the uploaded file
--
--  (2) The local filename supplied by the browser
--
--  (3) The content-type supplied by the browser
--
-- NOTE: You must move the file from the temporary location before the
-- 'Response' is sent. The temporary files are automatically removed
-- after the 'Response' is sent.
lookFile :: String -- ^ name of input field to search for
         -> ServerPart (FilePath, FilePath, ContentType) -- ^ (temporary file location, uploaded file name, content-type)
lookFile = S.lookFile

-- * Cookies

-- | gets the named cookie as a string
lookCookieValue :: String -> ServerPart String
lookCookieValue = S.lookCookieValue

-- | Add the list 'Cookie' to the 'Response'.
-- 
addCookies :: [(CookieLife, Cookie)] -> ServerPart ()
addCookies = S.addCookies

-- | Expire the named cookie immediately and set the cookie value to @\"\"@
--
-- > main = serve Nothing $
-- >   do expireCookie "name"
-- >      ok $ "The cookie has been expired."

expireCookie :: String -> ServerPart () 
expireCookie = S.expireCookie

-- * Headers

-- | Get a header out of the request.
getHeaderM :: String -> ServerPart (Maybe B.ByteString)
getHeaderM = S.getHeaderM

-- | Add headers into the response.  This method does not overwrite
-- any existing header of the same name, hence the name 'addHeaderM'.
-- If you want to replace a header use 'setHeaderM'.
addHeaderM :: String -> String -> ServerPart ()
addHeaderM = S.addHeaderM

-- | Set a header into the response.  This will replace an existing
-- header of the same name.  Use 'addHeaderM' if you want to add more
-- than one header of the same name.
setHeaderM :: String -> String -> ServerPart ()
setHeaderM = S.setHeaderM

-- * File Serving

-- | Serve files and directories from a directory and its subdirectories using 'sendFile'.
-- 
-- Usage:
--
-- > serveDirectory EnableBrowsing ["index.html"] "path/to/files/on/disk"
--
-- If the requested path does not match a file or directory on the
-- disk, then 'serveDirectory' calls 'mzero'.
--
-- If the requested path is a file then the file is served normally. 
--
-- If the requested path is a directory, then the result depends on
-- what the first two arguments to the function are.
--
-- The first argument controls whether directory browsing is
-- enabled.
--
-- The second argument is a list of index files (such as
-- index.html).
--
-- When a directory is requested, 'serveDirectory' will first try to
-- find one of the index files (in the order they are listed). If that
-- fails, it will show a directory listing if 'EnableBrowsing' is set,
-- otherwise it will return @forbidden \"Directory index forbidden\"@.
-- 
-- Here is an explicit list of all the possible outcomes when the
-- argument is a (valid) directory:
--
-- [@'DisableBrowsing', empty index file list@]
--
--  This will always return, forbidden \"Directory index forbidden\"
--
-- [@'DisableBrowsing', non-empty index file list@]
--
-- 1. If an index file is found it will be shown.
--
-- 2. Otherwise returns, forbidden \"Directory index forbidden\"
--
-- [@'EnableBrowsing', empty index file list@] 
--
-- Always shows a directory index.
--
-- [@'EnableBrowsing', non-empty index file list@]
--
-- 1. If an index file is found it will be shown
--
-- 2. Otherwise shows a directory index
--
-- see also: 'defaultIxFiles', 'serveFile'
serveDirectory :: Browsing    -- ^ allow directory browsing
               -> [FilePath]  -- ^ index file names, in case the requested path is a directory
               -> FilePath    -- ^ file/directory to serve
               -> ServerPart Response
serveDirectory = S.serveDirectory

-- | Serve a single, specified file. The name of the file being served is specified explicity. It is not derived automatically from the 'Request' url.
-- 
-- example 1:
-- 
--  Serve as a specific content-type:
--
-- > serveFile (asContentType "image/jpeg") "/srv/data/image.jpg"
--
--
-- example 2:
-- 
--  Serve guessing the content-type from the extension:
-- 
-- > serveFile (guessContentTypeM mimeTypes) "/srv/data/image.jpg"
--
-- If the specified path does not exist or is not a file, this function will return 'mzero'.
-- 
-- WARNING: No security checks are performed.
--
-- NOTE: alias for 'serveFileUsing' 'filePathSendFile'
serveFile :: (FilePath -> ServerPart String)   -- ^ function for determining content-type of file. Typically 'asContentType'
          -> FilePath                 -- ^ path to the file to serve
          -> ServerPart Response
serveFile = S.serveFile


-- | returns a specific content type, completely ignoring the 'FilePath' argument. 
--
-- Use this with 'serveFile' if you want to explicitly specify the
-- content-type.
--
-- see also: 'serveFile'
asContentType :: String  -- ^ the content-type to return
              -> (FilePath -> IO String)
asContentType = S.asContentType
