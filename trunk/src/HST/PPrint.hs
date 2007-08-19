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

module HST.PPrint where

import HST.AST 

import Text.PrettyPrint.HughesPJ

identifier (Identifier i) = text i
literal (StringLiteral s) = text $ "\'" ++ (concatMap stringEscape s) ++ "\'"

stringEscape c | c == '\'' = "''"
               | otherwise = [c]

primary (PrimaryLiteral p) = literal p
primary (PrimaryIdentifier i) = identifier i

keyword (Keyword k) = text $ k ++ ":"

message (KeywordMessage k p) = foldr (<+>) empty (zipWith (\k p -> keyword k <+> primary p) k p)

expression (BasicExpression p m) = primary p <+> message m
