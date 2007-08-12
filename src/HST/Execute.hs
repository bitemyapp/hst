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

module HST.Execute where

import HST.AST
import HST.Parse

data Object = TranscriptObject | StringObject String
    deriving Show

data MessageSend = MessageSend Selector [Object]

evaluatePrimary :: Primary -> Object
evaluatePrimary (PrimaryLiteral (StringLiteral s)) = StringObject s
evaluatePrimary (PrimaryIdentifier (Identifier "Transcript")) = TranscriptObject

--prepareMessage :: [Message] -> ([String],[Object])
--prepareMessage msg = unzip (map prepareMessageComponent msg)
--prepareMessageComponent (KeywordMessage (Keyword k) p) = (k, evaluatePrimary p)

prepareMessage :: [Message] -> [MessageSend]
prepareMessage msgs = map prepareMessage' msgs

prepareMessage' :: Message -> MessageSend
prepareMessage' (Message s args) = MessageSend s (map evaluatePrimary args)

evaluateExpression (BasicExpression prim msg) = sendMessage (evaluatePrimary prim) (prepareMessage msg)
    --where (sel, args) = prepareMessage msg

executeStatement (Expression e) = evaluateExpression e

executeElement (Initialization stmts) = sequence_ $ map executeStatement stmts
executeElement _ = return ()

execute ast = sequence_ $ map executeElement ast

sendMessage :: Object -> [MessageSend] -> IO ()
sendMessage TranscriptObject [(MessageSend ["show"] [(StringObject s)])] = putStrLn s
--sendMessage TranscriptObject ["show"] [(StringObject s)] = putStrLn s

