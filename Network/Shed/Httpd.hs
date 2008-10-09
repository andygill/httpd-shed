-- |
-- Module: Network.Shed.Httpd 
-- Copyright: Andy Gill
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: GHC
--
-- An simple, almost trivial web server. 
--
-- History:
--  Originally implmented for the Cherry chess processor.
--  Reused inside HERA and hpc-tracer

module Network.Shed.Httpd 
    ( Server
    , initServer
    , Request(..)
    , Response(..)
    ) where

import System.Posix
import System.Posix.Signals
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
    :: Int 				-- | The port number
    -> (Request -> IO Response) 	-- | The functionality of the Sever
    -> IO Server			-- | A Token for the Server
initServer portNo callOut = do
        installHandler sigPIPE Ignore Nothing    
        chan <- newChan
        sock  <- listenOn (PortNumber $ fromIntegral portNo)
        loopIO  
           (do (h,nm,port) <- accept sock
	       forkIO $ do 
                 tid <- myThreadId
                 ln <- hGetLine h
                 case words ln of
                   [mode,url,"HTTP/1.1"]  -> readHeaders h mode url []
                   _                      -> hClose h
                 return ()
           ) `finally` sClose sock
  where 
      loopIO m          = do m
                             loopIO m

      readHeaders h mode url hds = do
        line <- hGetLine h
        case span (/= ':') line of
          ("\r","") -> sendRequest h mode url hds
          (name,':':rest) -> readHeaders h mode url (hds ++ [(name,dropWhile Char.isSpace rest)])
          _ -> hClose h	-- strange format

      message code = show code ++ " " ++ 
                     case lookup code longMessages of
                       Just msg -> msg
                       Nothing -> "-"
      sendRequest h mode url hds = do
          resp <- callOut $ Request { reqMethod = mode
                                    , reqURI    = uri
                                    , reqArgs   = args
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
        where (uri,args) = splitup url

-- This should also cleanup names inside the uri itself
splitup url = case span (/= '?') url of
                 (path,'?':args) -> (path,splitargs args)
                 (path,_) -> (path,[])
  where
    splitargs xs = case span (/= '=') xs of
                     (index,'=':rest) ->
                       case span (/= '&') rest of
                         (value,'&':rest') -> (index,clean value) : splitargs rest'
                         (value,_)         -> (index,clean value) : []
                     _ -> []  

    clean ('%':d1:d2:cs) 
                 = Char.chr (read $ "0x" ++ [d1,d2])  : clean cs
    clean (c:cs) = c : clean cs
    clean []     = []

data Request = Request 
     { reqMethod  :: String	
     , reqURI     :: String
     , reqArgs    :: [(String,String)]
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
contentType msg =("Content-Type",msg)

------------------------------------------------------------------------------

longMessages = 
    [ (200,"OK")
    , (404,"Not Found")
    ]
