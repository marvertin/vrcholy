-- file Spec.hs
import Lib
import Uzemi

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException

  describe "Lib fst3 snd3 thr3" $ do
    it "fst3 returns first element of Tuple3" $ do
      fst3 (44, 58, 63) `shouldBe` (44 :: Int)

    it "returns the second element of an *arbitrary* Tuple3" $
      property $ \x  -> fst3 (x, 58, 50) == (x :: Int)

  describe "Uzemi" $ do
    describe "Uzemi.addMou" $ do

      it "correctly adds value"  $ do
        addMou 3 7 (Mou 60 80) `shouldBe` Mou 63 87

    describe "Uzemi.vystredMou" $ do

      it "return centre of 2 mous" $ do
        vystredMou [Mou 3 8, Mou 5 10]  `shouldBe` Mou 4 9
        
    describe "Uzemi.nejkratsiSpoj" $ do
      
        it "return empty path - all empty" $ do
            nejkratsiSpoj [] [] []  `shouldBe` Moustrov []

        it "return empty path - not connected" $ do
            nejkratsiSpoj [Mou 3 8] [Mou 20 40] [Mou 50 500]  `shouldBe` Moustrov []

        it "return empty path - neigthehood" $ do
            nejkratsiSpoj [Mou 1 2, Mou 1 3] [Mou 1 6, Mou 1 7] [Mou 1 4, Mou 1 5]  `shouldBe` Moustrov []
    
        it "return one point" $ do
            nejkratsiSpoj [Mou 1 2, Mou 1 3] [Mou 1 4, Mou 2 4, Mou 3 4, Mou 4 4] [Mou 1 5, Mou 1 6] 
               `shouldBe` Moustrov [Mou 1 4, Mou 2 4]

        it "return 4 points line"  $ do
         doTestNejkratsiSpoj
          ["............",
           "..11.    .22",
           "..111****222",
           "..11.    .22"]

        it "return 4 point column line"  $ do
            doTestNejkratsiSpoj
             ["..1.*....",
              "..11*22..",
              "..11*222.",
              "..11*22.."] 

        it "return single point"  $ do
            doTestNejkratsiSpoj
                [".11.....",
                "  11*  2 ",
                " .11 222.",
                "  11 22  "] 

        it "two path"  $ do
            doTestNejkratsiSpoj
                [".11**2..",
                "  1    22 ",
                " .11**222.",
                "  11  22  "] 
                        
        it "big block"  $ do
            doTestNejkratsiSpoj
                [
                "..............    *..........",
                "...............*****.........",
                "......111111111*****222222222",
                "......111111111*****222222222",
                "......111111111*****222222222",
                "......111111111*****222222222",
                "......111111111*****222222222",
                "......111111111*****222222222",
                "......111111111*****222222222",
                "......11111111.*****.22222222",
                "......1111111...***...2222222",
                "......1111111....*....2222222",
                "......1111111.........2222222"]
        
  describe "Uzemi - pole" $ do
    it "test of tesst creation" $ do
     pole ["1 21.",
           "2.11.",
           "*  2."] `shouldBe` ([Mou 1 1, Mou 1 4, Mou 2 3, Mou 2 4],
                                [Mou 1 5, Mou 2 2, Mou 2 5, Mou 3 5, Mou 3 1], 
                                [Mou 1 3, Mou 2 1, Mou 3 4], 
                                [Mou 3 1]
                               )
            


doTestNejkratsiSpoj p = let (o1, breh, o2, vysledek) = pole p
                         in nejkratsiSpoj o1 breh o2 `shouldBe` Moustrov vysledek

pole :: [String] -> ([Mou], [Mou], [Mou], [Mou])
pole p =
  let seznam = zip [1..] p >>= (\(x, line) -> map (\(y, znak) -> ((Mou x y),znak) ) (zip [1..] line))
      vyber :: Char -> [Mou]
      vyber c = map fst $ filter ((==c) . snd) seznam
  in (vyber '1', vyber '.' ++ vyber '*', vyber '2', vyber '*')    

   