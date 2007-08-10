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

module HST.Parse where

import HST.AST

import Control.Monad
import Text.ParserCombinators.Parsec

basicExpression = do
    p <- primary
    spaces
    m <- keywordMessage
    return $ BasicExpression p m

stringLiteral = StringLiteral `liftM` (quote >> stString >>~ quote)
quote = char '\''
stString = many $ choice [noneOf ['\''], try $ string "''" >> return '\'']

stLetter = choice [letter, char '_', digit]

identifierString = many1 stLetter

keyword = Keyword `liftM` (identifierString >>~ char ':')

identifier = Identifier `liftM` identifierString

primary = PrimaryIdentifier `liftM` identifier
      <|> PrimaryLiteral `liftM` stringLiteral

keywordMessage = do 
    k <- keyword
    spaces
    p <- primary
    return $ KeywordMessage k p

a >>~ b = a >>= \x -> b >> return x
        
