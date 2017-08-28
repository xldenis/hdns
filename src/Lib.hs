{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( readPacket
    ) where

import qualified Data.ByteString as B
import Data.Binary.Strict.Get
import Data.Word
import Data.Bits

data Message = Message{ tx_id :: Int
                      , flags :: Int
                      , questions :: [Question]
                      , answers :: [ResourceRecord]
                      } deriving (Show)

data Question = Question { name :: [B.ByteString]
                         , qtype :: Int
                         , qclass :: Int
                         } deriving (Show)

data ResourceRecord = ResourceRecord { rname :: [B.ByteString]
                                     , rtype :: Int
                                     , rclass :: Int
                                     , rttl :: Int
                                     , rdata :: ResourceData
                                     } deriving (Show)

data ResourceData =
    IPAddr (Int, Int, Int, Int)
  | Bytes B.ByteString deriving (Show)

readLabel = do
  n <- getWord8
  getByteString $ fromIntegral n

readOffset = do
  x <- getWord16be
  return $ fromIntegral $ x .&. 0x3fff

readEncodedString s pkt = do
    c <- lookAhead getWord8
    if c == 0
      then skip 1 >>= \_ -> return $ reverse s
      else if (c .&. 192) == 192
      then
             readOffset >>= \offset -> do
               let (x, _) = runGet (readEncodedString [] pkt) (B.drop offset pkt)
               case x of
                  Left x -> fail $ "can't find offset: " ++ show offset
                  Right x -> return x
      else
             readLabel >>= \o -> readEncodedString (o:s) pkt

readIP = do
  x <- getWord8
  y <- getWord8
  w <- getWord8
  z <- getWord8

  return $ IPAddr (fromIntegral x,fromIntegral y,fromIntegral w,fromIntegral z)

readAnswer bytes = do
  name <- readEncodedString [] bytes
  rtype <- getWord16be
  rclass <- getWord16be
  ttl <- getWord32be
  rd_len <- getWord16be

  rdata <- case rtype of
            1 -> do
              ip <- readIP
              return ip
            _ -> do
               b <- getByteString $ fromIntegral rd_len
               return $ Bytes b

  return $ ResourceRecord name (fromIntegral rtype) (fromIntegral rclass) (fromIntegral ttl) rdata


readQuestion bytes = do
  name <- readEncodedString [] bytes
  qtype <- getWord16be
  qclass <- getWord16be
  return $ Question name (fromIntegral qtype) (fromIntegral qclass)

readMessage bytes = do
  tx_id <- getWord16be
  flags <- getWord16be
  query_count <- getWord16be
  an_count <- getWord16be
  ns_count <- getWord16be
  ar_count <- getWord16be

  queries <- mapM (\_ -> readQuestion bytes) [1..query_count]

  answers <- mapM (\_ -> do readAnswer bytes) [1..an_count]
  return $ Message (fromIntegral tx_id) (fromIntegral flags) queries answers

readPacket :: String -> IO Message
readPacket s = do
  bytes <- B.readFile s
  let r =  runGet (readMessage bytes) bytes
  case (fst r) of
    Left error -> fail error
    Right x -> return x
