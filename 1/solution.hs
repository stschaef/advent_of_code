import System.IO
import Data.Char
import Data.List
import Data.Maybe

isNum :: Char -> Bool
isNum c = c >= '0' && c <= '9'

toNum :: String -> Int
toNum (n : []) = 11 * digitToInt n
toNum (n : ns) = (digitToInt n) * 10 + (digitToInt $ last ns)

digits :: [String]
digits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

digitWordIndexed :: String -> Int -> Int -> Maybe (Int, Int)
digitWordIndexed (c : cs) wordIdx startIdx =
  let word = digits !! wordIdx in
  if take (length word) (c : cs) == word
    then Just (startIdx, wordIdx + 1)
    else digitWordIndexed cs wordIdx (startIdx + 1)
digitWordIndexed [] _ _ = Nothing

lastDigitWordIndexed :: String -> Int -> Int -> Int -> Maybe (Int, Int)
lastDigitWordIndexed (c : cs) wordIdx startIdx len =
  let word = reverse (digits !! wordIdx) in
  if take (length word) (c : cs) == word
    then Just (len - startIdx - 1, wordIdx + 1)
    else lastDigitWordIndexed cs wordIdx (startIdx + 1) len
lastDigitWordIndexed [] _ _ _ = Nothing

makeLeftNum :: [Maybe (Int, Int)] -> Maybe (Int, Int)
makeLeftNum xs
  | xs == [] = Nothing
  | otherwise = minimum (filter isJust xs)

makeRightNum :: [Maybe (Int, Int)] -> Maybe (Int, Int)
makeRightNum xs
  | xs == [] = Nothing
  | otherwise = maximum (filter isJust xs)

firstDigitWords :: String -> Maybe (Int, Int)
firstDigitWords s =
  let potentialDigits = map (\idx -> digitWordIndexed s idx 0) [0..8] in
  let filteredDigits = filter (\x -> isJust x) potentialDigits in
  makeLeftNum filteredDigits

lastDigitWords :: String -> Maybe (Int, Int)
lastDigitWords s =
  let potentialDigits = map (\idx -> lastDigitWordIndexed (reverse s) idx 0 (length s)) [0..8] in
  let filteredDigits = filter (\x -> isJust x) potentialDigits in
  makeRightNum filteredDigits

firstDigitDigits :: String -> Int -> Maybe (Int, Int)
firstDigitDigits (c : cs) n
  | isDigit c = Just (n, digitToInt $ c)
  | otherwise = firstDigitDigits cs (n + 1)
firstDigitDigits [] _ = Nothing

lastDigitDigits :: String -> Int -> Maybe (Int, Int)
lastDigitDigits (c : cs) n
  | isDigit c = Just (n - 1, digitToInt $ c)
  | otherwise = lastDigitDigits cs (n - 1)
lastDigitDigits [] _ = Nothing

handleLine :: String -> Int
handleLine line =
  let firstDD = firstDigitDigits line 0 in
  let lastDD = lastDigitDigits (reverse line) (length line) in
  let firstDW = firstDigitWords line in
  let lastDW = lastDigitWords line in
  let firstD = makeLeftNum [firstDD, firstDW] in
  let lastD = makeRightNum [lastDD, lastDW] in
  snd (fromJust firstD) * 10 + snd (fromJust lastD)
  -- fst (fromJust lastDW)


main :: IO ()
main = do
  -- putStrLn $ show $ handleLine "mvhsixpptztjh13sixthree2"
  fileHandle <- openFile "input.txt" ReadMode
  contents <- hGetContents fileHandle
  let nums = map handleLine (lines contents)
  putStrLn $ show nums
  let sum = foldl (+) 0 nums
  putStrLn $ show sum
  hClose fileHandle
