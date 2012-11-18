module Sort(sort,sort') where

sort :: Ord a => [a] -> [a]
sort []     = []
sort (q:xs) = sort [ x | x<-xs, x<=q ] ++ [q] ++ sort [ x | x<-xs, not(x<=q) ]

sort' :: (a->a->Bool) -> [a] -> [a]
sort' (<=) []     = []
sort' (<=) (q:xs) = sort' (<=) [ x | x<-xs, x<=q ] ++ [q] ++ sort' (<=) [ x | x<-xs, not(x<=q) ]
