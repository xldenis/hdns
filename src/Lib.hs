{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( readPacket
    ) where

import           Data.Binary.Strict.Get
import           Data.Bits
import qualified Data.ByteString        as B
import           Data.Word
import Control.Monad

data Message = Message{ tx_id     :: Int
                      , flags     :: Int
                      , questions :: [Question]
                      , answers   :: [ResourceRecord]
                      } deriving (Show)

data Question = Question { name   :: [B.ByteString]
                         , qtype  :: Int
                         , qclass :: Int
                         } deriving (Show)

data ResourceRecord = ResourceRecord { rname  :: [B.ByteString]
                                     , rtype  :: Int
                                     , rclass :: Int
                                     , rttl   :: Int
                                     , rdata  :: ResourceData
                                     } deriving (Show)

data ResourceData =
    IPAddr (Int, Int, Int, Int)
  | Bytes B.ByteString deriving (Show)

readLabel = int8 >>= getByteString

readOffset = do
  x <- getWord16be
  return $ fromIntegral $ x .&. 0x3fff

readEncodedString s pkt = do
    c <- lookAhead getWord8
    when (c == 0) $ skip 1 *> return $ reverse s

    if (c .&. 192) == 192
      then
             readOffset >>= \offset -> do
               let (x, _) = runGet (readEncodedString [] pkt) (B.drop offset pkt)
               case x of
                  Left x  -> fail $ "can't find offset: " ++ show offset
                  Right x -> return x
      else
             readLabel >>= \o -> readEncodedString (o:s) pkt

readIP = do
  x <- int8
  y <- int8
  w <- int8
  z <- int8

  return $ IPAddr (x,y,w,z)

readAnswer bytes = do
  name <- readEncodedString [] bytes
  rtype <- int16
  rclass <- int16
  ttl <- int32
  rd_len <- int16

  rdata <- case rtype of
            1 -> readIP
            _ -> Bytes <$> (getByteString $ rd_len)

  return $ ResourceRecord name rtype rclass ttl rdata


readQuestion bytes = do
  name <- readEncodedString [] bytes
  qtype <- int16
  qclass <- int16
  return $ Question name qtype qclass

int16 = fromIntegral <$> getWord16be
int32 = fromIntegral <$> getWord32be
int8  = fromIntegral <$> getWord8

readMessage bytes = do
  tx_id <- int16
  flags <- int16
  query_count <- int16
  an_count <- int16
  ns_count <- int16
  ar_count <- int16

  queries <- replicateM query_count (readQuestion bytes)
  answers <- replicateM an_count (readAnswer bytes)

  return $ Message (tx_id) ( flags) queries answers

readPacket :: String -> IO Message
readPacket s = do
  bytes <- B.readFile s
  let r =  runGet (readMessage bytes) bytes
  case (fst r) of
    Left error -> fail error
    Right x    -> return x
