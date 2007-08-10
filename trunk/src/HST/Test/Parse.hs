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
import HST.Parse (basicExpression)
import HST.PPrint (expression)

import Control.Monad

import Test.QuickCheck

import Text.ParserCombinators.Parsec (parse) 
import Text.PrettyPrint.HughesPJ (render)

checkExpressionParser ast =
  case (parse basicExpression "" (render $ expression ast)) of
    Left _ -> False
    Right a -> ast == a
  where types = ast :: Expression

runChecks = quickCheck checkExpressionParser

instance Arbitrary Literal where
    arbitrary = liftM StringLiteral 
      $ sized (\n -> replicateM n $ choose ('\0','\255')) 
         
instance Arbitrary Expression where
    arbitrary = liftM2 BasicExpression arbitrary arbitrary

instance Arbitrary Message where
    arbitrary = liftM2 KeywordMessage arbitrary arbitrary 

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
