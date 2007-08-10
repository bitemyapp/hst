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

module HST.AST where

newtype Identifier = Identifier String
    deriving (Show, Eq)
newtype Literal = StringLiteral String
    deriving (Show, Eq)
data Primary = PrimaryLiteral Literal | PrimaryIdentifier Identifier
    deriving (Show, Eq)

newtype Keyword = Keyword String
    deriving (Show, Eq)
data Message = KeywordMessage Keyword Primary
    deriving (Show, Eq)

data Expression = BasicExpression Primary Message
    deriving (Show, Eq)
