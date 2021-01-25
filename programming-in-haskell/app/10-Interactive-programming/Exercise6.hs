import System.IO

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

clearLine :: IO ()
clearLine = putStr "\ESC[2K"

clearEnd :: IO ()
clearEnd = putStr "\ESC[0K"

backCursor :: IO ()
backCursor = putStr "\ESC[1D"

-- getChars :: Int -> IO String
-- getChars n = do
--   x <- getCh
--   if x == '\n'
--     then do
--       putChar x
--       return ['\n']
--     else
--       if x == '\b'
--         then do
--           backCursor
--           getChars (max 0 (n - 1))
--         else
--           if x == '\DEL'
--             then do
--               clearLine
--               return ['\DEL']
--             else do
--               putChar x
--               xs <- getChars n
--               return (x : xs)

getChars :: Int -> String -> IO String
getChars n xs = do
  x <- getChar
  if x == '\n'
    then do
      putChar x
      return (xs ++ [x])
    else
      if x == '\b'
        then do
          backCursor
          getChars (max 0 (n - 1)) xs
        else
          if x == '\DEL'
            then do
              clearEnd
              putStr (drop n xs)
              getChars n (take (n - 1) xs ++ drop n xs)
            else do
              putChar x
              getChars (n + 1) (xs ++ [x])

readLine :: IO String
readLine = getChars 0 ""

main = do
  xs <- readLine
  print xs
