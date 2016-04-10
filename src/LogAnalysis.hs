module LogAnalysis
    ( whatWentWrong
    , inOrder
    , build
    , insert
    , parse
    , parseMessage
    ) where

import Log

import Text.Parsec hiding (Error, parse)
import Text.Parsec.String

import qualified Text.Parsec as P

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map lmMessage
    . inOrder
    . build
    . filter ((> 50) . lmSeverity)
    . filter lmIsError

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lessors pivot greators) = concat
    [ inOrder lessors
    , [pivot]
    , inOrder greators
    ]

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x $ build xs

insert :: LogMessage -> MessageTree -> MessageTree
insert x Leaf = Node Leaf x Leaf
insert (Unknown _) mt = mt
insert lm (Node lessors pivot greators) =
    if lmTimeStamp lm < lmTimeStamp pivot
        then Node (insert lm lessors) pivot greators
        else Node lessors pivot $ insert lm greators

lmIsError :: LogMessage -> Bool
lmIsError (LogMessage (Error _) _ _ ) = True
lmIsError _ = False

lmSeverity :: LogMessage -> Int
lmSeverity (LogMessage (Error sv) _ _) = sv
lmSeverity _ = error "Can't get severity of non-errors"

lmTimeStamp :: LogMessage -> Int
lmTimeStamp (LogMessage _ ts _) = ts
lmTimeStamp _ = error "Can't get timestamp of Unknown"

lmMessage :: LogMessage -> String
lmMessage (LogMessage _ _ msg) = msg
lmMessage (Unknown _ ) = error "Can't get message of Unknown"

parse :: String -> [LogMessage]
parse = map parseMessage . lines

parseMessage :: String -> LogMessage
parseMessage = either err id . P.parse parser ""
  where
    err _ = Unknown "This is not in the right format"

parser :: Parser LogMessage
parser = choice
    [ parseLogLine 'E' $ Error <$> digitField
    , parseLogLine 'W' $ pure Warning
    , parseLogLine 'I' $ pure Info
    ]

parseLogLine
    :: Char               -- ^ Expected leading character
    -> Parser MessageType -- ^ How to parse the type field
    -> Parser LogMessage  -- ^ Resulting whole-line parser
parseLogLine c p = char c >> space >>
    LogMessage <$> p <*> digitField <*> many anyToken

digitField :: Parser Int
digitField = read <$> many digit <* space
