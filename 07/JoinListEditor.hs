module JoinListEditor where

import Scrabble
import Sized
import JoinList
import Buffer
import Editor (runEditor, editor)

main = runEditor editor (fromString . unlines $
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] :: (JoinList (Score, Size) String))
