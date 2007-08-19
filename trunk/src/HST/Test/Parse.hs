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

module HST.Test.Parse where

import HST.AST
import qualified HST.Parse as Parse 
import qualified HST.PPrint as PPrint

import Control.Monad

import Test.QuickCheck

import Text.ParserCombinators.Parsec (parse) 
import Text.PrettyPrint.HughesPJ (render)

checkExpressionParser ast =
  case (parse Parse.basicExpression "" (render $ PPrint.expression ast)) of
    Left _ -> False
    Right a -> ast == a
  where types = ast :: Expression

checkMessageParser kmsg =
  case (parse Parse.messages "" (render $ PPrint.message kmsg)) of
    Left _ -> False
    Right m -> kmsg == m
  where types = kmsg :: Message

runMessageChecks = quickCheck checkMessageParser
runChecks = quickCheck checkExpressionParser

instance Arbitrary Literal where
    arbitrary = liftM StringLiteral 
      $ sized (\n -> replicateM n $ choose ('\0','\255')) 
         
instance Arbitrary Expression where
    arbitrary = liftM2 BasicExpression arbitrary arbitrary

instance Arbitrary Message where
    arbitrary = oneof
        [ sized (\n -> liftM2 
            KeywordMessage 
              (replicateM (n+1) arbitrary) 
              (replicateM (n+1) arbitrary))
        , liftM UnaryMessage identifierGenerator
        ]
    

instance Arbitrary Primary where
   arbitrary = oneof 
       [ liftM PrimaryLiteral arbitrary
       , liftM PrimaryIdentifier arbitrary
       ]

alphaFreqList =
    [ (26, choose ('a', 'z'))
    , (26, choose ('A', 'Z'))
    , (1, elements ['_'])
    ]
digitFreqList = [ (10, choose ('0', '9')) ]

letter = frequency alphaFreqList
letterOrDigit = frequency $ alphaFreqList ++ digitFreqList

identifierGenerator = liftM2 (:) letter $ sized (\n -> replicateM n letterOrDigit) 
instance Arbitrary Identifier where
  arbitrary = liftM Identifier identifierGenerator

instance Arbitrary Keyword where
  arbitrary = liftM Keyword identifierGenerator
