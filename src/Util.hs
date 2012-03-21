-- |General purpose utility functions.
module Util where

import System.IO (hFlush, stdout)


-- |Print a newline.
lineBreak :: IO ()
lineBreak = putChar '\n'

-- |Print a string and then get some input.
prompt :: String -> IO String
prompt st = putStr st >> hFlush stdout >> getLine
