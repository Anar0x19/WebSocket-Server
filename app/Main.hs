import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions, acceptRequest, receiveData, sendTextData, Connection, PendingConnection)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as B

app :: Application
app req respond = respond $ responseLBS status200 [(CI.mk $ B.pack "Content-Type", B.pack "text/plain")] (BL.pack "Hello Anar !")

webSocketHandler :: PendingConnection -> IO ()  
webSocketHandler pending = do
    conn <- acceptRequest pending
    putStrLn "Client connected!"
    msg <- receiveData conn :: IO T.Text
    putStrLn $ "Received message: " ++ T.unpack msg
    sendTextData conn (T.pack "Hello, client!")

main :: IO ()
main = run 8080 $ websocketsOr defaultConnectionOptions webSocketHandler app


-- ghc -o WebSocketServer Main.hs -package text -package wai -package warp -package wai-websockets -package websockets -package http-types -package bytestring -package case-insensitive
