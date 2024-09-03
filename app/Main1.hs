{-# LANGUAGE OverloadedStrings #-}

import Network.Wai (Application, responseLBS, requestMethod, rawPathInfo)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status400, status405)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions, acceptRequest, receiveData, sendTextData, Connection, PendingConnection)
import Network.Wai.Parse (parseRequestBody, lbsBackEnd, fileName, fileContent)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as B
import System.Directory (createDirectoryIfMissing)
import System.IO (readFile)

app :: Application
app req respond
    | requestMethod req == "GET" && rawPathInfo req == "/" = do
        htmlContent <- BL.readFile "Style.html" 
        respond $ responseLBS status200 [("Content-Type", "text/html")] htmlContent
    | requestMethod req == "POST" && rawPathInfo req == "/upload" = do

        (params, files) <- parseRequestBody lbsBackEnd req
        case files of
            [] -> respond $ responseLBS status400 [("Content-Type", "text/plain")] "No file uploaded."
            ((_, fileInfo):_) -> do
                let filePath = "./uploads/" ++ B.unpack (fileName fileInfo)
                createDirectoryIfMissing True "./uploads"
                BL.writeFile filePath (fileContent fileInfo)
                respond $ responseLBS status200 [("Content-Type", "text/plain")] "File uploaded successfully."
    | otherwise = respond $ responseLBS status405 [("Content-Type", "text/plain")] "Method not allowed."

webSocketHandler :: PendingConnection -> IO ()
webSocketHandler pending = do
    conn <- acceptRequest pending
    putStrLn "Client connected!"
    msg <- receiveData conn :: IO T.Text
    putStrLn $ "Received message: " ++ T.unpack msg
    sendTextData conn (T.pack "Hello, client!")

main :: IO ()
main = run 8080 $ websocketsOr defaultConnectionOptions webSocketHandler app











-- ghc -o Test Main1.hs -package wai -package warp -package http-types -package wai-websockets -package websockets -package wai-extra -package wai-middleware-static -package text -package bytestring -package case-insensitive -package directory
