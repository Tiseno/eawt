import           Control.Monad      (when)
import           Data.Bifunctor     (first)
import           Data.Char          (isDigit, isSpace)
import           Data.List          (partition)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (hPutStr, stderr)

unexpectedCharacter :: Char -> String -> String
unexpectedCharacter u e =
  "error: expected " ++ e ++ " but found '" ++ [u] ++ "'"

endOfInput :: String
endOfInput = "error: unexpected end of input"

newtype TimeValue =
  TimeValue Int

newTimeValue :: Int -> Int -> TimeValue
newTimeValue hours minutes = TimeValue $ hours * 60 + minutes

getHours :: TimeValue -> Int
getHours (TimeValue t) = t `div` 60

getMinutes :: TimeValue -> Int
getMinutes (TimeValue t) = t `mod` 60

showTime :: TimeValue -> String
showTime value@(TimeValue i) =
  let sign = f i 0 "-"
      hours = abs $ getHours value
      minutes = abs $ getMinutes value
      hPad = f hours 10 "0"
      mPad = f minutes 10 "0"
   in sign ++ hPad ++ show hours ++ ":" ++ mPad ++ show minutes
  where
    f v c s =
      if v < c
        then s
        else ""

parseValue :: String -> Either String (TimeValue, String)
parseValue [] = Left endOfInput
parseValue (':':s) = first (newTimeValue 0) <$> parseInt s
parseValue s = do
  (hours, s') <- parseInt s
  case s' of
    (':':s'') -> first (newTimeValue hours) <$> parseInt s''
    _         -> Right (newTimeValue hours 0, s')

parseInt :: String -> Either String (Int, String)
parseInt [] = Left endOfInput
parseInt s@(x:_)
  | isDigit x = Right $ first read $ span isDigit s
parseInt (x:_) = Left $ unexpectedCharacter x "number"

parseAndFoldTerms :: (TimeValue, String) -> Either String TimeValue
parseAndFoldTerms (acc, s) =
  case dropWhile isSpace s of
    [] -> Right acc
    s' -> do
      (op, v, s'') <- parseTerm s'
      parseAndFoldTerms (acc `op` v, s'')

parseTerm ::
     String
  -> Either String (TimeValue -> TimeValue -> TimeValue, TimeValue, String)
parseTerm s = do
  (op, s') <- parseOperator s
  (v, s'') <- parseValue $ dropWhile isSpace s'
  Right (op, v, s'')

parseOperator ::
     String -> Either String (TimeValue -> TimeValue -> TimeValue, String)
parseOperator [] = Left endOfInput
parseOperator ('+':s) = Right (add, s)
  where
    add (TimeValue t1) (TimeValue t2) = TimeValue (t1 + t2)
parseOperator ('-':s) = Right (sub, s)
  where
    sub (TimeValue t1) (TimeValue t2) = TimeValue (t1 - t2)
parseOperator (x:_) = Left $ unexpectedCharacter x "operator"

parseAndCalculate :: String -> Either String TimeValue
parseAndCalculate s = parseValue (dropWhile isSpace s) >>= parseAndFoldTerms

isFlag :: String -> Bool
isFlag ('-':'-':_) = True
isFlag _           = False

main :: IO ()
main = do
  (flags, input) <- partition isFlag . words . unwords <$> getArgs
  when ("--help" `elem` flags) $ do
    putStrLn $ unlines
      [ "Usage: eawt [EXPRESSION]"
      , ""
      , "Performs addition and subtraction on time values."
      , ""
      , "Example usage:"
      , ""
      , "eawt 3 + 1:00 + :15 - 0:32"
      , "03:43"
      ]
    exitSuccess
  case parseAndCalculate $ unwords input of
    Left err -> do
      hPutStr stderr err
      exitFailure
    Right ok -> do
      putStr $ showTime ok
      exitSuccess
