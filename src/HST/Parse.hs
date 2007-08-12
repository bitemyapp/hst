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
    m <- messages
    return $ BasicExpression p m

stringLiteral = StringLiteral `liftM` (stringDelimiter >> stString >>~ stringDelimiter)
stString = many $ choice [noneOf ['\''], try $ string "''" >> return '\'']

quotedSelector = SelectorLiteral `fmap` (char '#' >> identifierString)

literal = choice [ stringLiteral, quotedSelector ]

comment = commentDelimiter >> nonCommentDelimiter >>~ commentDelimiter >>= (return . Comment) 
nonCommentDelimiter = many $ noneOf ['"']

{-
 - Primarys
 -}
primary = PrimaryIdentifier `liftM` identifier
      <|> PrimaryLiteral `liftM` literal


identifier = Identifier `liftM` identifierString
identifierString = many1 stLetter
stLetter = choice [letter, char '_', digit]

{-
 - Messages
 -}
messages :: GenParser Char st [Message]
messages = (try keywordMessage >>= \m -> return [m])
        <|> (manyTill unaryMessage (try keywordMessage) >>= \umsgs -> keywordMessage >>= \kmsgs -> return (umsgs ++ [kmsgs]))


keywordMessage :: GenParser Char st Message
keywordMessage = keywordMessage' [] []
keywordMessage' :: [Keyword] -> [Primary] -> GenParser Char st Message
keywordMessage' sel arg = (lexeme keyword >>= \k ->
                      lexeme primary >>= \p ->
                      keywordMessage' (sel++[k]) (arg++[p]))
                      <|> (return $ Message sel arg)

keyword = identifierString >>~ char ':'

unaryMessage = lexeme identifierString >>= \msg -> return $ Message [msg] []

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
    elementDelimiter
    cls <- identifierString
    spaces
    ctr <- option InstanceMethod (string "class" >> return ClassMethod)
    spaces
    string "methodsFor:"
    spaces
    category <- stringLiteral
    elementDelimiter
    mthd <- method ctr
    spaces
    elementDelimiter
    elementDelimiter
    return $ MethodDefinition mthd


initializerDefinition = statements
programInitializer = initializerDefinition >>~ elementDelimiter  >>= (return . Initialization) 

programElement = (comment >>~ elementDelimiter
             <|> methodDefinition
             <|> programInitializer) 

smalltalkFile = many (programElement >>~ spaces) >>~ eof

{-
 - Simple Tokens
 -}
 
commentDelimiter = char '"'
elementDelimiter = lexeme $ char '!'
stringDelimiter = char '\'' 

{-
 - Utility Functions
 -}
a >>~ b = a >>= \x -> b >> return x

lexeme p = p >>~ spaces
        
