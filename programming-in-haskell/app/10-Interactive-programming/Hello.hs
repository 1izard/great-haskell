import System.IO

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

getBack :: IO Bool
getBack = do
  x <- getCh
  putStrLn [x]
  return (x == '\b')

main = do
  hSetBuffering stdin NoBuffering
  res <- getBack
  print res