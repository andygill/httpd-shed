import Network.Shed.Httpd

main = do 
  initServer 8091 $ \ req -> do
                     print req
                     if reqURI req == "/"
                        then 
                          return $ Response 200 [] "<root dir>"
                        else do
                          str <- readFile ("." ++ (reqURI req))
                          return $ Response 200 [] str
  return ()
