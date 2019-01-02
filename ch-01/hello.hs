sayHello :: String -> IO()
sayHello x = putStrLn("Hello, " ++ x ++ "!")

triple :: Int -> Int
triple x = x * 3

areaCircle :: Int -> Float
areaCircle r = pi * (fromIntegral r * fromIntegral r)
