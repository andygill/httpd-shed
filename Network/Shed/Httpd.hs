-- |
-- Module: Network.Shed.Httpd 
-- Copyright: Andy Gill
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: GHC
--
-- A trivial web server, original used in the cherry chess processor.
--

module Network.Shed.Httpd 
    ( Server
    , initServer
    , Request(..)
    , Response(..)
    , queryToArguments
    ) where

--import System.Posix
--import System.Posix.Signals
import Network.URI
import Network
import System.IO 
import Control.Monad 
import Control.Concurrent 
import Control.Exception as Exc
import Control.Concurrent.Chan
import qualified Data.List as List
import qualified Data.Char as Char

data Server = Server

initServer 
    :: Int 				-- ^ The port number
    -> (Request -> IO Response) 	-- ^ The functionality of the Sever
    -> IO Server			-- ^ A token for the Server
initServer portNo callOut = do
--        installHandler sigPIPE Ignore Nothing    
        chan <- newChan
        sock  <- listenOn (PortNumber $ fromIntegral portNo)
        loopIO  
           (do (h,nm,port) <- accept sock
	       forkIO $ do 
                 tid <- myThreadId
                 ln <- hGetLine h
                 case words ln of
                   [mode,uri,"HTTP/1.1"]  -> 
                       case parseURIReference uri of
                         Just uri' -> readHeaders h mode uri' []
                         _ -> do print uri 
                                 hClose h
                   _                      -> hClose h
                 return ()
           ) `finally` sClose sock
  where 
      loopIO m          = do m
                             loopIO m

      readHeaders h mode uri hds = do
        line <- hGetLine h
        case span (/= ':') line of
          ("\r","") -> sendRequest h mode uri hds
          (name,':':rest) -> readHeaders h mode uri (hds ++ [(name,dropWhile Char.isSpace rest)])
          _ -> hClose h	-- strange format

      message code = show code ++ " " ++ 
                     case lookup code longMessages of
                       Just msg -> msg
                       Nothing -> "-"
      sendRequest h mode uri hds = do
          resp <- callOut $ Request { reqMethod = mode
                                    , reqURI    = uri
                                    , reqHeaders = hds
                                    , reqBody   = ""
                                    }
          hPutStr h $ "HTTP/1.1 " ++ message (resCode resp) ++ "\r\n"               
          hPutStr h $ "Connection: close\r\n"
          sequence [ hPutStr h $
                             hdr ++ ": " ++ val ++ "\r\n"
                     | (hdr,val) <- resHeaders resp 
                   ]
          hPutStr h $ "Content-Length: " ++ 
                                   show (length (resBody resp)) ++ "\r\n"
          hPutStr h $ "\r\n"
          hPutStr h $ (resBody resp) ++ "\r\n"
          hClose h

-- | Takes an escaped query, optionally starting with '?', and returns an unescaped index-value list.
queryToArguments :: String -> [(String,String)]
queryToArguments ('?':rest) = queryToArguments rest
queryToArguments input = findIx input
   where
     findIx = findIx' . span (/= '=') 
     findIx' (index,'=':rest) = findVal (unEscapeString index) rest
     findIx' _ = []

     findVal index = findVal' index . span (/= '&')
     findVal' index (value,'&':rest) = (index,unEscapeString value) : findIx rest
     findVal' index (value,[])       = [(index,unEscapeString value)]
     findVal' _ _ = []

data Request = Request 
     { reqMethod  :: String	
     , reqURI     :: URI
     , reqHeaders :: [(String,String)]
     , reqBody    :: String
     }
     deriving Show

data Response = Response
    { resCode	 :: Int
    , resHeaders :: [(String,String)]
    , resBody    :: String
    }
     deriving Show

addCache :: Int -> (String,String)
addCache n = ("Cache-Control","max-age=" ++ show n)

noCache :: (String,String)
noCache = ("Cache-Control","no-cache")

-- examples include "text/html" and "text/plain"

contentType :: String -> (String,String)
contentType msg = ("Content-Type",msg)

------------------------------------------------------------------------------

longMessages = 
    [ (200,"OK")
    , (404,"Not Found")
    ]
