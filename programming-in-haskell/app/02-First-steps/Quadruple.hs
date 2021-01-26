double x = x + x

quadruple x = double (double x)

factorial n = product [1 .. n]

average ns = sum ns `div` length ns

main = do
  print (quadruple 10)
  print (take (double 2) [1, 2, 3, 4, 5])
  print (factorial 10)
  print (average [1, 2, 3, 4, 5])