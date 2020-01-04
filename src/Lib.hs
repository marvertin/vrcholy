module Lib
    ( Mou, Mnm, Bod
    ) where

-- Souřadnice bodu po třech vteřinách (0,0) odpovídá N0 E0
type Mou = (Int, Int)

-- Nadmořská výška v metrech
type Mnm = Int

-- Bod jako kombinace souřadnic a nadmořské výšky
type Bod = (Mou, Mnm)
