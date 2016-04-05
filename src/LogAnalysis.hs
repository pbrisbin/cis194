module LogAnalysis
    ( insert
    , parse
    , parseMessage
    ) where

import Log

import Control.Monad (void)
import Text.Parsec hiding (Error, parse)
import Text.Parsec.String

import qualified Text.Parsec as P

insert :: LogMessage -> MessageTree -> MessageTree
insert x Leaf = Node Leaf x Leaf
insert (Unknown _) mt = mt
insert lm (Node lessors pivot greators) =
    if lmTimeStamp lm < lmTimeStamp pivot
        then Node (insert lm lessors) pivot greators
        else Node lessors pivot $ insert lm greators

lmTimeStamp :: LogMessage -> Int
lmTimeStamp (LogMessage _ ts _) = ts
lmTimeStamp _ = error "Can't get timestamp of Unknown"

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
    void $ charField 'E'

    LogMessage
        <$> (Error <$> digitField)
        <*> digitField
        <*> many anyToken

parseWarn :: Parser LogMessage
parseWarn = do
    void $ charField 'W'

    LogMessage
        <$> pure Warning
        <*> digitField
        <*> many anyToken

parseInfo :: Parser LogMessage
parseInfo = do
    void $ charField 'I'

    LogMessage
        <$> pure Info
        <*> digitField
        <*> many anyToken

charField :: Char -> Parser Char
charField c = char c <* space

digitField :: Parser Int
digitField = read <$> many digit <* space
