{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

(|>) :: a -> (a -> b) -> b
(|>) a f = f a

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage s = case words s of
    ("I" : timestamp : ss)                -> LogMessage Info (read timestamp) (unwords ss)
    ("W" : timestamp : ss)                -> LogMessage Warning (read timestamp) (unwords ss)
    ("E" : errorWeight : timestamp : ss)  -> LogMessage (Error (read errorWeight)) (read timestamp) (unwords ss)
    _                                     -> Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

-- Exercise 2
compareByTimestamp :: LogMessage -> LogMessage -> Ordering
compareByTimestamp (Unknown _) (Unknown _)                  = EQ
compareByTimestamp (Unknown _) (LogMessage _ _ _)           = LT
compareByTimestamp (LogMessage _ _ _) (Unknown _)           = GT
compareByTimestamp (LogMessage _ t1 _) (LogMessage _ t2 _)  = compare t1 t2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree   = messageTree
insert msg Leaf                  = Node Leaf msg Leaf
insert msg (Node sx node dx)     = case compareByTimestamp msg node of
   LT    -> Node (insert msg sx) node dx
   _     -> Node sx node (insert msg dx)

-- Exercise 3
build :: [LogMessage] -> MessageTree
build []      = Leaf
build (l:ls)  = insert l (build ls)

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf               = []
inOrder (Node sx node dx)  = inOrder sx ++ (node : inOrder dx)

-- Exercise 5
isError :: LogMessage -> Bool
isError (LogMessage (Error _) _ _) = True
isError _                          = False

hasSeverityOverThreshold :: Int -> LogMessage -> Bool
hasSeverityOverThreshold threshold (LogMessage (Error severity) _ _) = severity >= threshold
hasSeverityOverThreshold _ _                                 = False

extractMessage :: LogMessage -> String
extractMessage (LogMessage _ _ msg) = msg
extractMessage (Unknown msg)        = msg

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msg = filter (\it -> isError it && hasSeverityOverThreshold 50 it) msg
    |> build
    |> inOrder
    |> map extractMessage



