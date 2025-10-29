(\f :: (Int -> Int) -> \g :: (Int -> Int) -> \x :: Int ->
  (f (g x)) + x)
(\n :: Int -> n + 1)
(\n :: Int -> n + 10)
5
