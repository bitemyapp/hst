{-
   Copyright 2007 Levi Stephen

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

module Main( main ) where

import Prelude
import System.Environment
import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Char8 as BS

import HST.Execute
import HST.Parse

strictReadFile :: FilePath -> IO String
strictReadFile f = BS.readFile f >>= \file -> return $ BS.unpack file

main = head `fmap` getArgs >>= \fileName ->
       strictReadFile fileName >>= \file ->
       case parse basicExpression fileName file of
           Left  err -> putStrLn $ show err
           Right ast -> evaluateExpression ast
