--import Prelude hiding (Monad, (>>=), (>>), return, (<*>), (<$>), Applicative, Functor, fmap)
import Data.Char
import Eval hiding (num)

{-
class Functor f where
  fmap::(a->b)->(f a)->(f b)

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

class Applicative f where
  (<*>)::f(a->b)->(f a)->(f b)
  pure::a->(f a)

class Monad m where
  return::a->m a
  (>>=)::(m a)->(a->m b)->(m b)

(>>)::(Monad m)=>(m a)->(m b)->(m b)
a >> b = a >>= (\_->b)
-}

(<<)::(Monad m)=>(m a)->(m b)->(m a)
a << b = a >>= (\v->b >>= (\_->return v))

--type Parser a = String -> [(String,a)]
data Parser a = Parser (String -> [(String,a)])
parse::Parser a->String -> [(String,a)]
parse (Parser p) = p

instance Functor Parser where
  fmap f (Parser p) = Parser (\s -> [(s',f a)|(s',a)<-p s])

instance Applicative Parser where
  --(Parser x) <*> (Parser y) = Parser (\s -> concat [ [ (s'', a a') |(s'', a')<-y s'] |(s',a)<-x s])
  x <*> y = x >>= (\f-> y >>= (\a -> return (f a)))
  pure = produce

instance Monad Parser where
  return a = Parser (\s->[(s,a)])
  (Parser p) >>= f = Parser (\s -> concat [ parse (f a) s' |(s',a)<- p s] )

(<++>)::Parser String->Parser String->Parser String
x <++> y = (++) <$> x <*> y

(<:>)::Parser a->Parser [a]->Parser [a]
x <:> y = (:) <$> x <*> y

(<||>)::Parser a-> Parser a->Parser a
(<||>) (Parser a) (Parser b) = Parser (\c -> a c ++ b c)

(<|>)::Parser a-> Parser a->Parser a
(<|>) (Parser a) (Parser b) = Parser (\s->case a s of
                                            [] -> b s
                                            as -> as)

(>>=?)::(Parser a)->(a->Parser b)->(Parser b)
p >>=? f = p >>= (\a-> manyChar ' ' >> produce a >>= f)

(>>?)::(Parser a)->(Parser b)->(Parser b)
a >>? b = a >> manyChar ' ' >> b

(<<?)::(Parser a)->(Parser b)->(Parser a)
a <<? b = a << manyChar ' ' << b

produce::a->Parser a
produce x = Parser (\s->[(s,x)])

failure::Parser a
failure = Parser (\s->[])

item::Parser Char
item = Parser (\i -> case i of
                (c:cs) -> [(cs,c)]
                [] -> []
              )

eos::Parser Char
eos = Parser (\i -> case i of
                (c:cs) -> []
                [] -> [(i,'\0')]
              )

char::Char->Parser Char
char c' = Parser (\i -> case i of
                    (c:cs) -> if c==c' then [(cs,c)] else []
                    [] -> []
                  )
notchar::Char->Parser Char
notchar c' = Parser (\i -> case i of
                    (c:cs) -> if c/=c' then [(cs,c)] else []
                    [] -> []
                  )

manyChar::Char->Parser [Char]
--manyChar c = return [] <|> ((:) <$> char c <*> manyChar c)
--manyChar c = return [] <|> (char c >>= (\c'-> manyChar c >>= (\cs'-> return (c':cs'))))
manyChar = many . char

many::Parser a->Parser [a]
many p = (p <:> many p) <|> return []

string::String -> Parser String
string [] = produce []
string (c:cs) = char c >>= (\a->string cs >>= (\a'->produce (c:cs)))

checkNext::Parser a->Parser a
checkNext (Parser p) = Parser (\s->[(s,a)|(s',a)<-p s])

digit::Parser Int
digit = digitToInt <$> ( char '0' <|> char '1' <|> char '2' <|> char '3' <|> char '4' <|> char '5' <|> char '6' <|> char '7' <|> char '8' <|> char '9' )

orchar::[Char]->Parser Char
orchar = foldr (\c acc-> char c <|> acc) failure

num::Parser (Fix Expr)
{-
num = (digit >>= (\a->num >>= (\(In (Num a'))->produce (In (Num (a*10+a'))))))
  <|> (digit >>= (\a-> eos <|> checkNext (orchar [' ', ')' ,'+']) >> produce (In (Num a))))
-}
num = (do
        a <- digit
        (In (Num a')) <- num
        return (In (Num (10*a+a'))))
  <|> (do
        a <- digit
        eos <|> checkNext (orchar [' ', ')' ,'+', '*'])
        return (In (Num a)))


subexpr::Parser (Fix Expr)
subexpr = char '(' >>? expr <<? char ')'

expr::Parser (Fix Expr)
{-
expr = num
  <|> (num <|> subexpr >>=? (\a-> char '+' >>? (num  <|> subexpr) >>= (\a'-> produce (In (Plus a a')))))
-}
expr = num
  <||> (do
        a <- num <||> subexpr
        manyChar ' '
        char '+'
        manyChar ' '
        a' <- num  <||> subexpr
        return (In (Plus a a'))
      )
  <||> (do
        a <- num <||> subexpr
        manyChar ' '
        char '*'
        manyChar ' '
        a' <- num  <||> subexpr
        return (In (Times a a'))
      )

{- [["*", "\"], ["+", "-"]...] -}

-- 1 * 8 \ 9
-- Plus (Mul 1 8) 9

expr' = manyChar ' ' >> expr << manyChar ' ' << eos

parseToExpr::Parser (Fix Expr)->String->Fix Expr
parseToExpr p = snd . head . (parse p)

parseAndEvalExpr::String->Int
parseAndEvalExpr = eval . (parseToExpr expr')

{-
<digit> := ( 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 )
<num> :=  <digit> <num> | <digit>
<expr> := <num> | <subexpr> '+' <subexpr>
<subexpr> := '(' <expr> ')' | <num>
-}
