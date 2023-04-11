{-# LANGUAGE NamedFieldPuns #-}

--
-- Prioritní fronta ja založena na MaxQueue, což je halda, ale umožňujeme odebírat.
-- Odebíráme tak, že si ve skutečnosti nedržíme provy, ale dvojice s handly a při každém odběru handle odebereme
--

module PrioritniFronta
    ( 
        Prifron,
        Handle,

        empty,
        fromList,

        insert,
        insertH,
        deleteH,

        maxView, -- Tímto odebíráme z fronty.

        test
    ) where


import qualified Data.PQueue.Max as Q      -- vlastní fronta
import qualified Data.IntSet as S          -- budeme si zde držet zrušence

type Handle = Int

type Item a = (a, Handle) -- všude musíme přidat handle, aby se dalo rušit

data Prifron a = Prifron { qq :: Q.MaxQueue (Item a), rusenci :: S.IntSet, lastHand :: Handle }
  deriving Show

  
que = Q.findMax $ Q.fromList [12,56,89,78,100,230,1,2,5]


insertH :: Ord a => a -> Prifron a -> (Handle, Prifron a)
insertH x prifron = 
    let hand = lastHand prifron + 1
    in (hand, prifron { qq = Q.insert (x, hand) (qq prifron), lastHand = hand } )

insert :: Ord a => a -> Prifron a -> Prifron a
insert x prifron = prifron { qq = Q.insert (x, 0) (qq prifron) } 

deleteH :: Handle -> Prifron a -> Prifron a
deleteH hand prifron@(Prifron {rusenci}) = prifron { rusenci = S.insert hand rusenci }

maxView ::  Ord a => Prifron a -> Maybe (a, Prifron a)
maxView prifron@(Prifron {qq, rusenci} ) = 
            let totoZrusit = map snd $ Q.takeWhile (\(_, handle) -> S.member handle rusenci) qq
            in obal (S.fromList totoZrusit) <$> Q.maxView (Q.drop (length totoZrusit) qq)
      where 
        obal :: S.IntSet ->  (Item a, Q.MaxQueue (Item a)) -> (a, Prifron a)
        obal zrusit ((x, _), qu) = (x, prifron { qq = qu, rusenci = rusenci S.\\ zrusit })

empty :: Prifron a
empty = Prifron { qq = Q.empty, rusenci = S.empty, lastHand = 0 }

fromList :: Ord a => [a] -> Prifron a
fromList list =  let qq = Q.fromList $ fmap (\x -> (x, 0)) list
                 in Prifron { qq, rusenci = S.empty, lastHand = 0 } 


test :: IO ()
test = do
       let (_, fr1) = insertH "bob" empty
       putStrLn $ show fr1
       let (ha2, fr2) = insertH "cecil" fr1
       putStrLn $ show fr2
       let (ha3, fr3) = insertH "david" fr2
       putStrLn $ show fr3
       let (ha4, fr4) = insertH "anna" fr3
       putStrLn $ show fr4
       let (_, fr5) = insertH "emil" fr4
       putStrLn $ show fr5

       let fr5x = fromList ["helena", "pavel", "martin", "lenka", "olina"]
       let (Just (x1, g1)) = maxView fr5
       putStrLn $ show x1 ++ " --------- " ++ show g1

       let g1a = deleteH ha2 . deleteH ha3 . deleteH ha3 $ g1

       let (Just (x2, g2)) = maxView g1a
       putStrLn $ show x2 ++ " --------- " ++ show g2
       --let (Just (x3, g3)) = maxView g2
       putStrLn $ show (maxView g2)
