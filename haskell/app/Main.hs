import           Data.Bifunctor     (first)
import           Data.Char          (isDigit, isSpace)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (hPutStr, stderr)

data Error
  = UnexpectedCharacter
      { expected       :: String
      , unexpectedChar :: Char
      }
  | EndOfInput

instance Show Error where
  show (UnexpectedCharacter {expected = e, unexpectedChar = u}) =
    "error: expected " ++ e ++ " but found '" ++ [u] ++ "'"
  show EndOfInput = "error: unexpected end of input"

newtype TimeValue =
  TimeValue Int

instance Show TimeValue where
  show value@(TimeValue i) =
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

newTimeValue :: Int -> Int -> TimeValue
newTimeValue hours minutes = TimeValue $ hours * 60 + minutes

getHours :: TimeValue -> Int
getHours (TimeValue t) = t `div` 60

getMinutes :: TimeValue -> Int
getMinutes (TimeValue t) = t `mod` 60

parseAndCalculate :: String -> Either Error TimeValue
parseAndCalculate s = parseValue (dropWhile isSpace s) >>= parseAndFoldTerms

parseValue :: String -> Either Error (TimeValue, String)
parseValue [] = Left EndOfInput
parseValue (':':s) = first (newTimeValue 0) <$> parseInt s
parseValue s = do
  (hours, s') <- parseInt s
  case s' of
    (':':s'') -> first (newTimeValue hours) <$> parseInt s''
    _         -> Right (newTimeValue hours 0, s')

parseInt :: String -> Either Error (Int, String)
parseInt [] = Left EndOfInput
parseInt s@(x:_)
  | isDigit x = Right $ first read $ span isDigit s
parseInt (x:_) = Left $ UnexpectedCharacter "number" x

parseAndFoldTerms :: (TimeValue, String) -> Either Error TimeValue
parseAndFoldTerms (acc, s) =
  case dropWhile isSpace s of
    [] -> Right acc
    s' -> do
      (op, v, s'') <- parseTerm s'
      parseAndFoldTerms (acc `op` v, s'')

parseTerm ::
     String
  -> Either Error (TimeValue -> TimeValue -> TimeValue, TimeValue, String)
parseTerm s = do
  (op, s') <- parseOperator s
  (v, s'') <- parseValue $ dropWhile isSpace s'
  Right (op, v, s'')

parseOperator ::
     String -> Either Error (TimeValue -> TimeValue -> TimeValue, String)
parseOperator [] = Left EndOfInput
parseOperator ('+':s) = Right (add, s)
  where
    add (TimeValue t1) (TimeValue t2) = TimeValue (t1 + t2)
parseOperator ('-':s) = Right (sub, s)
  where
    sub (TimeValue t1) (TimeValue t2) = TimeValue (t1 - t2)
parseOperator (x:_) =
  Left UnexpectedCharacter {expected = "operator", unexpectedChar = x}

main :: IO ()
main = do
  input <- unwords <$> getArgs
  case parseAndCalculate input of
    Left err -> hPutStr stderr (show err) >>= const exitFailure
    Right ok -> putStr (show ok) >>= const exitSuccess
