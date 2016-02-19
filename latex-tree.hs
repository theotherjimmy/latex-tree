#!/usr/bin/runhaskell

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import Data.Attoparsec.ByteString.Lazy hiding (take)
import Data.Attoparsec.Combinator
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BSS
import Data.Foldable (traverse_)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text (unpack, pack)
import Data.Text.IO (putStrLn, putStr)
import Data.Maybe
import Options.Applicative hiding (option)
import Prelude hiding (takeWhile, putStrLn, putStr)
import System.FilePath.Posix ((<.>))
import Data.Monoid ((<>))

data Arguments = Arguments { inputfile :: String }

argParser = Arguments <$> argument str (metavar "INPUT")

fileParser = manyTill (choice [ Just . BSS.copy . (<> ".tex") <$> (string "input{" *> takeTill (125 ==))
                              , takeTill (92 ==) *> pure Nothing <* skip (92 ==) ]) (takeTill (92 ==) *> endOfInput)

data Tree = Tree { nodeVal  :: !BSS.ByteString
                 , children :: [Tree]} deriving Show

parseTree :: BSS.ByteString -> IO [Tree]
parseTree foo = let !i = doParsing $ BSL.fromStrict foo in
  zipWith Tree i <$> (mapM (BSS.readFile . unpack . decodeUtf8) i >>= mapM (parseTree))
  where doParsing = fromMaybe [] . fmap catMaybes . maybeResult . parse fileParser


fancyPrint = fancyPrint' 0 ""
  where fancyPrint' num prefix node = do traverse_ putStr $ reverse $ take num (prefix : genList num)
                                         putStrLn $ decodeUtf8 $ nodeVal node
                                         traverse_ (uncurry (fancyPrint' (num + 1))) $ zip (reverse $ take (length $ children node) childOrder) $ children node
        childOrder = "└──" : repeat "├──"
        genList 0 = []
        genList 1 = []
        genList 2 = ["│  "]
        genList n = take (n-2) (repeat "   ") <> genList 2

main = execParser opts >>= fromArgs parseTree >>= fancyPrint
  where opts = info (helper <*> argParser) (fullDesc)
        fromArgs fn args = Tree (encodeUtf8 $ pack $ inputfile args)  <$> (BSS.readFile (inputfile args) >>= fn)
