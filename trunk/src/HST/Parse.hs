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
    m <- (try keywordMessage
      <|> unaryMessages) 
    return $ BasicExpression p m

stringLiteral = StringLiteral `liftM` (quote >> stString >>~ quote)
quote = char '\''
stString = many $ choice [noneOf ['\''], try $ string "''" >> return '\'']

quotedSelector = SelectorLiteral `fmap` (char '#' >> identifierString)

literal = choice [ stringLiteral, quotedSelector ]

comment = commentDelimiter >> nonCommentDelimiter >>~ commentDelimiter >>= (return . Comment) 
commentDelimiter = char '"'
nonCommentDelimiter = many $ noneOf ['"']

stLetter = choice [letter, char '_', digit]

identifierString = many1 stLetter

keyword = Keyword `liftM` (identifierString >>~ char ':')

identifier = Identifier `liftM` identifierString

primary = PrimaryIdentifier `liftM` identifier
      <|> PrimaryLiteral `liftM` literal

unaryMessage = identifierString >>~ spaces >>= (return . UnaryMessage)
unaryMessages = do
  umsgs <- manyTill unaryMessage (try keywordMessage)
  kmsgs <- keywordMessage
  return $ umsgs ++ kmsgs

keywordMessage = many ((do 
    k <- keyword
    spaces
    p <- primary
    return $ KeywordMessage k p) >>~ spaces)


expression = basicExpression
statements = statements' []
statements' s = (returnStatement >>= \ret -> return (s ++ [ret]))
             <|> (expression >>= \expr -> return (s ++ [Expression expr]))

returnStatement = char '^' >> expression >>= (return . Return)

unarySelector = identifierString >>~ spaces >>= \selector -> return [selector]

method ctr = unarySelector >>= \selector ->
    statements >>= \statements ->
    return $ ctr selector statements

methodDefinition = do
    char '!' 
    cls <- identifierString
    spaces
    ctr <- option InstanceMethod (string "class" >> return ClassMethod)
    spaces
    string "methodsFor:"
    spaces
    category <- stringLiteral
    char '!'
    spaces
    mthd <- method ctr
    spaces
    char '!'
    spaces
    char '!'
    return $ MethodDefinition mthd


initializerDefinition = statements
programInitializer = initializerDefinition >>~ char '!' >>= (return . Initialization) 

programElement = (comment >>~ char '!'
             <|> methodDefinition
             <|> programInitializer) 
             


--choice
--  [ comment >>~ option ' ' (char '!')
--  , programInitializer
--  ]
smalltalkFile = many (programElement >>~ spaces) >>~ eof

a >>~ b = a >>= \x -> b >> return x
        
