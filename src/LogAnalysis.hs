module LogAnalysis
    ( parse
    , parseMessage
    ) where

import Log

import Text.Parsec hiding (Error, parse)
import Text.Parsec.String

import qualified Text.Parsec as P

parse :: String -> [LogMessage]
parse = map parseMessage . lines

parseMessage :: String -> LogMessage
parseMessage = either err id . P.parse parser ""
  where
    err _ = Unknown "This is not in the right format"

parser :: Parser LogMessage
parser = parseError <|> parseWarn <|> parseInfo

parseError :: Parser LogMessage
parseError = do
    charField 'E'

    LogMessage
        <$> (Error <$> digitField)
        <*> digitField
        <*> many anyToken

parseWarn :: Parser LogMessage
parseWarn = do
    charField 'W'

    LogMessage
        <$> pure Warning
        <*> digitField
        <*> many anyToken

parseInfo :: Parser LogMessage
parseInfo = do
    charField 'I'

    LogMessage
        <$> pure Info
        <*> digitField
        <*> many anyToken

charField :: Char -> Parser Char
charField c = char c <* space

digitField :: Parser Int
digitField = read <$> many digit <* space
