-- |General purpose utility functions.
module Util where

import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))


-- |Print a newline.
lineBreak :: IO ()
lineBreak = putChar '\n'

-- |Print a string and then get some input.
prompt :: String -> IO String
prompt st = do hSetBuffering stdout LineBuffering
               putStr st
               getLine
