--makeSum n = head . dropWhile ((>n) . sum) . iterate lower
--    where
--        lower xs = map (min (maximum xs - 1)) xs

--differences fs = zipWith (-) fs (tail fs)
--differences = ap (zipWith (-)) tail
--differences = (zipWith (-)) <*> tail

--differences = zipWith (-) =<< tail

dec x = x-1

--lower = do
--    n <- getTarget
--    lower' n

makeSum n = head . dropWhile ((>n) . sum) . iterate lower
    where lower = (map . min) =<< (dec . maximum)

--items = [[1,2,3],[3,2],[1],[1,2],[3,2,3,2]]
--n = 100

--scaled = (map . scaleWith =<< sum) . map length
--    where scaleWith total = max 1 . (`div`total) . (n*)
