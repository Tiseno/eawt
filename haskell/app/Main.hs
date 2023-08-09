{-# LANGUAGE PatternSynonyms #-}

import           Control.Monad      (when)
import           Data.Char          (isDigit, isSpace)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (stderr, hPutStr)

main :: IO ()
main = do
  args <- getArgs
  when ("--format" `elem` args) $ do
    let input = unwords $ filter (/= "--format") args
    case format input of
      Ok ok -> do
        putStr ok
        exitSuccess
      Err err -> do
        hPutStr stderr $ show err
        exitFailure
  let input = unwords args
  case evaluate input of
    Ok ok -> do
      putStr $ show ok
      exitSuccess
    Err err -> do
      hPutStr stderr $ show err
      exitFailure

data Token
  = Number String
  | Colon
  | Plus
  | Minus

instance Show Token where
  show (Number n) = n
  show Colon      = ":"
  show Plus       = "+"
  show Minus      = "-"

type Result a b = Either a b

{-# COMPLETE Err, Ok #-}

pattern Err :: a -> Either a b

pattern Err x = Left x

pattern Ok :: b -> Either a b

pattern Ok x = Right x

data Error
  = UnexpectedCharacter
      { unexpectedChar :: Char
      }
  | UnexpectedToken
      { expected        :: String
      , unexpectedToken :: Token
      }
  | EndOfInput

instance Show Error where
  show (UnexpectedCharacter {unexpectedChar = u}) =
    "error: unexpected character '" ++ [u] ++ "'"
  show (UnexpectedToken {expected = e, unexpectedToken = t}) =
    "error: expected " ++ e ++ " but found " ++ show t
  show EndOfInput = "error: unexpected end of input"

format :: String -> Result Error String
format input = do
  tokens <- tokenize input
  (first, following) <- parse tokens
  Ok $
    show first ++
    concatMap (\(op, value) -> " " ++ show op ++ " " ++ show value) following

tokenize :: String -> Result Error [Token]
tokenize input = fst <$> tokenizeR ([], input)

tokenizeR :: ([Token], String) -> Result Error ([Token], String)
tokenizeR (tokens, []) = Ok (tokens, [])
tokenizeR (tokens, input@(x:xs)) =
  case x of
    ':' -> tokenizeR (tokens ++ [Colon], xs)
    '+' -> tokenizeR (tokens ++ [Plus], xs)
    '-' -> tokenizeR (tokens ++ [Minus], xs)
    _
      | isSpace x -> tokenizeR (tokens, xs)
    _
      | isDigit x ->
        let (digits, rest) = span isDigit input
         in tokenizeR (tokens ++ [Number digits], rest)
    ch -> Err $ UnexpectedCharacter {unexpectedChar = ch}

newtype TimeValue =
  TimeValue Int

instance Show TimeValue where
  show value =
    let sign =
          if isNegative value
            then "-"
            else ""
        hours = abs $ getHours value
        minutes = abs $ getMinutes value
        hourPadding =
          if hours < 10
            then "0"
            else ""
        minutePadding =
          if minutes < 10
            then "0"
            else ""
     in sign ++
        hourPadding ++ show hours ++ ":" ++ minutePadding ++ show minutes

newTimeValue :: Int -> Int -> TimeValue
newTimeValue hours minutes = TimeValue $ hours * 60 + minutes

fromHours :: Int -> TimeValue
fromHours hours = newTimeValue hours 0

fromMinutes :: Int -> TimeValue
fromMinutes = newTimeValue 0

add :: TimeValue -> TimeValue -> TimeValue
add (TimeValue t1) (TimeValue t2) = TimeValue (t1 + t2)

subtract :: TimeValue -> TimeValue -> TimeValue
subtract (TimeValue t1) (TimeValue t2) = TimeValue (t1 - t2)

getHours :: TimeValue -> Int
getHours (TimeValue t) = t `div` 60

getMinutes :: TimeValue -> Int
getMinutes (TimeValue t) = t `mod` 60

isNegative :: TimeValue -> Bool
isNegative (TimeValue t) = t < 0

data Operator
  = Add
  | Subtract

instance Show Operator where
  show Add      = "+"
  show Subtract = "-"

evaluate :: [Char] -> Result Error TimeValue
evaluate input = do
  tokens <- tokenize input
  parsed <- parse tokens
  Ok $ calculate parsed

parse :: [Token] -> Result Error (TimeValue, [(Operator, TimeValue)])
parse tokens = do
  (first, rest) <- parseValue tokens
  (following, _) <- parseFollowing rest
  Ok (first, following)

parseValue :: [Token] -> Either Error (TimeValue, [Token])
parseValue ((Number hours):Colon:(Number minutes):xs) =
  Ok (newTimeValue (read hours) (read minutes), xs)
parseValue ((Number hours):xs) = Ok (fromHours (read hours), xs)
parseValue (Colon:(Number minutes):xs) = Ok (fromMinutes (read minutes), xs)
parseValue [] = Err EndOfInput
parseValue (x:_) =
  Err UnexpectedToken {expected = "number", unexpectedToken = x}

parseOperator :: [Token] -> Either Error (Operator, [Token])
parseOperator (Plus:xs) = Ok (Add, xs)
parseOperator (Minus:xs) = Ok (Subtract, xs)
parseOperator [] = Err EndOfInput
parseOperator (x:_) =
  Err UnexpectedToken {expected = "operator", unexpectedToken = x}

parseFollowing :: [Token] -> Either Error ([(Operator, TimeValue)], [Token])
parseFollowing [] = Ok ([], [])
parseFollowing tokens = do
  (op, rest) <- parseOperator tokens
  (value, rest') <- parseValue rest
  (following, rest'') <- parseFollowing rest'
  Ok ((op, value) : following, rest'')

calculate :: (TimeValue, [(Operator, TimeValue)]) -> TimeValue
calculate (first, following) = foldl doOp first following
  where
    doOp a (Add, b)      = add a b
    doOp a (Subtract, b) = Main.subtract a b
