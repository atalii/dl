{-# LANGUAGE OverloadedStrings #-}

module ProtoUtils (readQuery, writeResponse, processQuery) where

import Control.Lens.Getter
import Control.Lens.Setter
import qualified Data.ByteString as B
import Data.Function
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Encoding
import Network.Socket
import Network.Socket.ByteString
import qualified NetworkUtils as NU
import qualified Proto.Messaging as M
import Proto.Messaging_Fields as MF

readQuery :: Socket -> IO M.Query
readQuery sock = do
  len <- recv sock 2
  len' <- NU.decode16BE $ B.unpack len

  payload <- recv sock len'
  let query = decodeMessage payload
  case query of
    Left err -> error err -- TODO: exception
    Right query' -> return query'

writeResponse :: Socket -> M.Response -> IO ()
writeResponse sock response = do
  let response' = encodeMessage response
  let l = NU.encode16BE . toInteger $ B.length response'
  sendAll sock (B.pack l)
  sendAll sock response'

-- TODO: this should be an Either and not an IO
processQuery :: M.Query -> IO M.Response
processQuery query = case query ^. MF.maybe'content of
  Just (M.Query'Syn' qSyn) -> mkSynAck qSyn
  Nothing -> undefined

mkSynAck :: M.Query'Syn -> IO M.Response
mkSynAck s = case v of
  "0.1.0" -> return ack
  _ -> undefined -- TODO: raise
  where
    v = s ^. version
    ack :: M.Response
    ack =
      defMessage
        & maybe'content
          ?~ M.Response'Synack (defMessage & version .~ v)
