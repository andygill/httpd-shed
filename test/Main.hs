import Network.Shed.Httpd

main = do 
  initServer 8091 $ \ req -> do
                     print req
                     str <- readFile (tail (reqURI req))
                     return $ Response 200 False "text/plain" str
  return ()
