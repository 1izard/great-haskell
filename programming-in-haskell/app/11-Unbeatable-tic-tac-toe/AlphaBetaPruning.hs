data Tree a = Leaf a | Node [Tree a]

max' :: Int
max' = 1000

min' :: Int
min' = -1000

values :: [Int]
values = [3, 5, 6, 9, 1, 2, 0, -1]

tree :: Tree Int
tree =
  Node
    [ Node
        [ Node
            [ Leaf 3,
              Leaf 5
            ],
          Node
            [ Leaf 6,
              Leaf 9
            ]
        ],
      Node
        [ Node
            [ Leaf 1,
              Leaf 2
            ],
          Node
            [ Leaf 0,
              Leaf (-1)
            ]
        ]
    ]

minimax :: Bool -> Tree Int -> Int -> Int -> Int -> Int
minimax _ (Leaf x) _ _ _ = x
minimax _ (Node []) a b best = best
minimax True (Node xs) a b best =
  if b <= a
    then best
    else minimax True (Node (tail xs)) a' b best'
  where
    best' = max best (minimax False (head xs) a b max')
    a' = max a best'
minimax False (Node xs) a b best =
  if b <= a
    then best
    else minimax False (Node (tail xs)) a b' best'
  where
    best' = min best (minimax True (head xs) a b min')
    b' = min b best'

main = do
  print (minimax True tree min' max' min') -- 5
