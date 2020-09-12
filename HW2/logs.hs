{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
n,

parseMessage :: String -> LogMessage
parseMessage str = let wordList = words str in
                   case wordList of
                     ("I":ts:msg) -> LogMessage Info (read ts) (unwords msg)
                     ("W":ts:msg) -> LogMessage Warning (read ts) (unwords msg)
                     ("E":lvl:ts:msg) -> LogMessage (Error (read lvl)) 
                                            (read ts) (unwords msg)
                     _ -> Unknown (unwords wordList)



parse :: String -> [LogMessage]
parse = map parseMessage . lines



insert :: LogMessage -> MessageTree -> MessageTree
insert lmsg@LogMessage{} Leaf = Node Leaf lmsg Leaf
insert lmsg1@(LogMessage _ ts1 _) (Node left lmsg2@(LogMessage _ ts2 _) right)
  | ts1 > ts2 = Node left lmsg2 (insert lmsg1 right)
  | otherwise = Node (insert lmsg1 left) lmsg2 right
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build [] = Leaf 
build (lmsg:msgs) = insert lmsg (build msgs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lmsg right) = inOrder left  ++ [lmsg] ++ inOrder right

severity :: Int -> LogMessage -> Bool
severity lowest (LogMessage (Error x) _ _)
  | lowest < x = True
  | otherwise  = False
severity _ _ = False

extractMessage :: [LogMessage] -> [String]
extractMessage (LogMessage _ _ msg:msgs) = msg : extractMessage msgs
extractMessage _ = [] 

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = extractMessage . inOrder . build . filter( severity 50 )

