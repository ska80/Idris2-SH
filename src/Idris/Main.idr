module Idris.Main

import Data.List

import Idris.Driver

import Compiler.Common
import Compiler.Scheme.Chez
import Compiler.Scheme.Racket
import Compiler.Scheme.Gambit
import Compiler.ES.Javascript
import Compiler.ES.Node

main : IO ()
main = mainWithCodegens [
     ("chez", codegenChez)
   , ("racket", codegenRacket)
   , ("gambit", codegenGambit)
   , ("node", codegenNode)
   , ("javascript", codegenJavascript)
  ]
