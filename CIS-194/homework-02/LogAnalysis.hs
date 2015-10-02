{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage msg = case (words msg) of
  ("I":time:string) -> LogMessage Info (read time) (unwords string)
  ("W":time:string) -> LogMessage Warning (read time) (unwords string)
  ("E":code:time:string) -> LogMessage (Error (read code)) (read time) (unwords string)
  _ -> Unknown msg

parse :: String -> [LogMessage]
parse content = map parseMessage $ lines content

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node left node right)
  | (time msg) <= (time node) = Node (insert msg left) node right
  | otherwise                 = Node left node (insert msg right)
  where
    time (LogMessage _ t _) = t
    time _                  = 0

build :: [LogMessage] -> MessageTree
build []     = Leaf
build (x:xs) = insert x (build xs) 

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = map message $ filter important xs
    where message (LogMessage _ _ m) = m
          message (Unknown _) = ""
          important (LogMessage (Error sev) _ _) = (sev >= 30)
          important _ = False
