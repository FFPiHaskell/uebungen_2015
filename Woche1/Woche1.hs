module Woche1 where

--- Aufgabe 1.1

-- point-free
caesar :: Int -> String -> String
caesar n = map (toEnum . (+n) . fromEnum)

--- Aufgabe 1.2

filterFile :: (String -> Bool) -> FilePath -> FilePath -> IO ()
filterFile pred input output = do
  cont <- readFile input
  writeFile output $ (unlines . (filter pred) . lines) cont

-- Hackerstolz: In einer Zeile. Das müsst ihr noch nicht unbedingt nachvollziehen können 
filterFile' :: (String -> Bool) -> FilePath -> FilePath -> IO ()
filterFile' p iF oF = readFile iF >>= (writeFile oF) . ((unlines . filter p . lines))

--- Aufgabe 1.3

-- (a)

-- Siehe "Sieb des Eratosthenes"
infinitePrimes :: [Integer]
infinitePrimes = sieb [2..] 
  where
    sieb :: [Integer] -> [Integer]
    sieb []     = []
    sieb (i:is) = i : sieb [ x | x <- is, mod x i /= 0]

-- :-P
finitePrimes :: Integer -> [Integer]
finitePrimes n = takeWhile (<= n) infinitePrimes

-- (b)

primfaktoren :: Integer -> [Integer]
primfaktoren 1 = []
primfaktoren n = factor : primfaktoren (div n factor)
  where
    factor :: Integer
    factor = case safeHead prims of
                  (Just p) -> p
                  Nothing  -> n

    prims :: [Integer]
    prims = [ k | k <- finitePrimes (n-1), n `mod` k == 0]

-- Sichere Version von Head, die nicht fehlschlagen kann
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = (Just x)

-- Unsafe, schmeißt einen Fehler für falsche Eingaben
goldbach :: Integer -> (Integer, Integer)
goldbach n | (odd n || n < 2) = error "Keine gerade natürliche Zahl!"
           | otherwise        = case safeHead possibilities of
                                     (Just pair) -> pair
                                     Nothing     -> error "Goldbachvermutung wiederlegt!"
  where
    possibilities = [(k, n-k) | k <- finitePrimes (n-1), (n-k) `elem` finitePrimes (n-1)]
