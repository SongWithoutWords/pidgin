module TestInput where

defNotL1 :: String
defNotL1 =
  "myNot(Bln b) -> Bln => false if b else true"

defNotL2 :: String
defNotL2 =
  "myNot(Bln b) -> Bln =>\n\
  \    false if b else true"

defNotL3 :: String
defNotL3 =
  "myNot(Bln b) -> Bln =>\n\
  \    false if b else\n\
  \    true"

defNotL6 :: String
defNotL6 =
  "myNot(Bln b) -> Bln =>\n\
  \\n\
  \    false if b else\n\
  \\n\
  \    true\n\
  \\n"

defFactorial :: String
defFactorial =
  "factorial(Int n) -> Int =>\n\
  \    1 if n <= 0 else n * factorial(n-1)"

