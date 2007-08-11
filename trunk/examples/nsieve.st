"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy
    modified by Eliot Miranda *"!

Object subclass: #Tests
   instanceVariableNames: ''
   classVariableNames: ''
   poolDictionaries: ''
   category: 'Shootout'!

!Tests class methodsFor: 'platform'!
arg
   ^Smalltalk arguments first asInteger! !

!Tests class methodsFor: 'platform'!
stdin
   ^FileStream stdin! !

!Tests class methodsFor: 'platform'!
stdinSpecial
   ^self stdin bufferSize: 4096! ! 

!Tests class methodsFor: 'platform'!
stdout
   ^FileStream stdout! !

!Tests class methodsFor: 'platform'!
stdoutSpecial
   ^self stdout bufferSize: 4096! !

!Stream methodsFor: 'platform'!
print: number digits: decimalPlaces
   | n s |
   n := 0.5d0 * (10 raisedToInteger: decimalPlaces negated).
   s := ((number sign < 0) ifTrue: [number - n] ifFalse: [number + n]) printString.
   self nextPutAll: (s copyFrom: 1 to: (s indexOf: $.) + decimalPlaces)! !

!Stream methodsFor: 'platform'!
print: number paddedTo: width
   | s |
   s := number printString.
   self nextPutAll: (String new: (width - s size) withAll: $ ), s! !

!Integer methodsFor: 'platform'!
asFloatD
   ^self asFloat! !

!Tests class methodsFor: 'benchmark scripts'!
nsieve
   | n |
   n := self arg.
   (n < 2) ifTrue: [n := 2].
   self primeBenchmarkFor: n to: self stdout using: Array.
   ^''! !

!Tests class methodsFor: 'benchmarking'!
nsieve: n using: arrayClass 
   | count isPrime |
   count := 0.
   isPrime := arrayClass new: n withAll: true.
   2 to: n do:
      [:i | 
      (isPrime at: i) ifTrue: 
         [i + i to: n by: i do:
            [:k | isPrime at: k put: false].
         count := count + 1]].
   ^count! !

!Tests class methodsFor: 'benchmarking'!
primeBenchmarkFor: v to: output using: arrayClass
   v to: v - 2 by: -1 do:
      [:n| | m |
      m := (2 raisedTo: n) * 10000.
      output
         nextPutAll: 'Primes up to ';
         print: m paddedTo: 8;
         print: (self nsieve: m using: arrayClass) paddedTo: 9; nl
      ]! !

Tests nsieve!
