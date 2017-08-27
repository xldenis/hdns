{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( readPacket
    ) where

import qualified Data.ByteString as B
import Data.Binary.Strict.Get
import Data.Word
import Data.Bits
import qualified Data.Map as Map

-- data Question = Question{ name :: String,
--                           q_type :: QuestionType
--                         }

data Message = Message{ tx_id :: Int
                      , flags :: Int
                      , questions :: [Question]
                      } deriving (Show)

readLabel :: Get B.ByteString
readLabel = do
  n <- getWord8
  getByteString $ fromIntegral n

readEncodedString :: [B.ByteString] -> Map.Map Int B.ByteString  -> Get [B.ByteString]
readEncodedString s decodeMap = do
    c <- lookAhead getWord8
    pos <- bytesRead
    if c == 0
      then skip 1 >>= \_ -> return $ reverse s
      else if c .&. 192 == 192
      then
              let offset = fromIntegral $ c .&. 63 in
                case Map.lookup offset decodeMap of
                  Nothing -> fail $ "can't find offset: " ++ show offset
                  Just x -> readEncodedString (x:s) decodeMap
      else
             readLabel >>= \o ->
                             readEncodedString (o:s) (Map.insert pos o decodeMap)

data Question = Question { name :: [B.ByteString]
                         , qtype :: Int
                         , qclass :: Int
                         } deriving (Show)

readQuestion :: Get Question
readQuestion = do
  name <- readEncodedString [] Map.empty
  qtype <- getWord16be
  qclass <- getWord16be
  return $ Question name (fromIntegral qtype) (fromIntegral qclass)

readHeader :: Get Message
readHeader = do
  tx_id <- getWord16be
  flags <- getWord16be
  query_count <- getWord16be
  an_count <- getWord16be
  ns_count <- getWord16be
  ar_count <- getWord16be

  queries <- mapM (\_ -> readQuestion) [1..query_count]
  -- answers <- mapM (\_ -> readEncodedString [] Map.empty) [1..]
  return $ Message (fromIntegral tx_id) (fromIntegral flags) queries

readPacket :: String -> IO Message
readPacket s = do
  bytes <- B.readFile s
  let r =  runGet readHeader bytes
  case (fst r) of
    Left error -> fail error
    Right x -> return x
