{-# LANGUAGE OverloadedStrings #-}
module Main where

import Pipes
import qualified Pipes.Text as Text
import qualified Pipes.Text.IO as Text
import System.IO
import Lens.Family
import qualified Pipes.Group as Group
import Debug.Trace
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Monoid
import Data.Char

textIssueFixed = (LT.null . LT.takeWhileEnd isSpace $ "foobar") == True

appendAt :: Int -> T.Text -> T.Text -> (T.Text,T.Text)
appendAt i appendBS bs = let (first, rest) = T.splitAt i bs in (first <> appendBS <> rest,rest)

fixedWidthLine :: [Int] -> T.Text -> T.Text
fixedWidthLine offsets bs = fst $ go offsets ("" :: T.Text,bs)
  where go :: [Int] -> (T.Text,T.Text) -> (T.Text,T.Text)
        go [] x = x
        go (x:xs) (acc,rest) = let (newAcc,newRest) = T.splitAt x rest in go xs (acc <> (fst $ appendAt x "|" newAcc),newRest)

toDelimPipe :: Monad m => [Int] -> Pipe T.Text T.Text m r
toDelimPipe offsets = do
    chunk <- await
    let text = fixedWidthLine offsets chunk
    let wasNull = T.null text
    if wasNull
      then toDelimPipe offsets
      else do yield text
              cat

-- TODO figure out how to test a pipe (I'm sure pipes-text or any other libraries test their pipes)
toDelimPipeTrace :: Monad m => [Int] -> Pipe T.Text T.Text m r
toDelimPipeTrace offsets = do
    chunk <- await
    let text = trace ("chunk was: '" ++ T.unpack chunk ++ "'") ((fixedWidthLine offsets) chunk)
    let wasNull = trace ("was it null? text: '" <> T.unpack text <> "'\n , T.null text => " <> show (T.null text)) $ T.null text
    if wasNull
      then (toDelimPipe offsets)
      else do yield text
              cat

fileToFileTest1024lines = do
  withFile "test1024.fixedwidth"  ReadMode  $ \hIn  ->
    withFile "test1024.delimited" WriteMode $ \hOut ->
    -- the group will end on the 2047'th character and the pipe will be inserted in the incorrect position
    -- as can be seen on the second line where y's begin at character 2047 and immediately after a pipe delimiter is inserted
    runEffect $ over Text.lines (Group.maps (>-> toDelimPipe [682,682])) (Text.fromHandle hIn) >-> Text.toHandle hOut

main :: IO ()
main = do
  putStrLn "hello world"
