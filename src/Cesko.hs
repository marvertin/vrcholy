-- Souřadnice kilometrové sítě pro česko.
-- Kilometrová síť je zpracovávána speciálně pro 
module Cesko (
     stredniSirkaCeska,
     qocientZmenseniSirkyVCesku,
     mouToKm
) where


import HraniceCeska
import Gps
import Data.Monoid
import Uzemi


stredniSirkaCeska :: Double
stredniSirkaCeska =  (getSum $ mconcat $ map (Sum . snd) hraniceCeska) / (fromIntegral $ length hraniceCeska)


namorniMile = 1.852
boduNaMinutuVCesku = fromIntegral boduNaStupen / 60

-- Zkreslení způsobené tím, že v zeměpisné délce jsou stupně kratší než v šířce
-- Není dobré toto použít, jak je použito, neboť to zkresluje i vlastní kreslené tvary, z kružnic dělá elipsy. Na souřadnice budeme muset jít jinak.
-- Udává, kolikrát je kratší jedna úhlová jedntka (stupeň, minata, vteřina) ve směru zeměpisné délky a to na úrovni Česka
qocientZmenseniSirkyVCesku = cos (stredniSirkaCeska / 180 * pi) 

-- Převedení souřadnic souřadnic bodů na souřadnice kilometrové.
-- Současná implementace je zjednodušující pro česko.
mouToKm :: Mou -> (Double, Double)
mouToKm (Mou x y) =
    (fromIntegral x / boduNaMinutuVCesku * namorniMile * qocientZmenseniSirkyVCesku, 
     fromIntegral y / boduNaMinutuVCesku * namorniMile  )
