import Text.ParserCombinators.Parsec

file::Parser Char Int [[Int]]
file = do
          hori
          x <- lines'
          eof
          a <- getState
          return x

hori::GenParser Char Int [[Int]]
hori = do
          char '-'
          horicols
          char '\n'
          return []

horicols::GenParser Char Int [[Int]]
horicols = (do
          char '-'
          char '-'
          updateState (+1)
          horicols)
          <|> return []


lines'::GenParser Char Int [[Int]]
lines' = (do
          l <- line
          ls <- lines'
          return (l:ls))
        <|> return []

line::GenParser Char Int [Int]
line = do
          a <- getState
          char '|'
          x <- nums a
          char '\n'
          count (2*a+1) (char '-')
          char '\n'
          return x

nums::Int->GenParser Char Int [Int]
nums 0 = return []
nums n = (do
          x <- num
          char '|'
          xs <- nums (n-1)
          return (x:xs))


num::GenParser Char Int Int
num = read <$> many1 digit


parseSquare :: String -> Either ParseError [[Int]]
parseSquare input = runParser file 0 "Incorrect shape" input

{-
-------
|1|2|3|
-------
|4|5|6|
-------
|7|8|9|
-------

---
|1|
---
|2|
---

-}

eg1x2 = "---\n|1|\n---\n|2|\n---\n"

eg3x3 = "-------\n\
        \|1|2|3|\n\
        \-------\n\
        \|4|5|6|\n\
        \-------\n\
        \|7|8|9|\n\
        \-------\n"

eg3x3mk1 = "----\n\
           \|1|2|3|\n\
           \-------\n\
           \|4|5|6|\n\
           \-------\n\
           \|7|8|9|\n\
           \-------\n"

eg3x3mk2 = "-------\n\
           \|1|2|3|\n\
           \-------\n\
           \|4|5|\n\
           \-------\n\
           \|7|8|9|\n\
           \-------\n"
