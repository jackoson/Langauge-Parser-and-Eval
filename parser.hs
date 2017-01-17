import Data.Char
import Eval hiding (num)

(<<)::(Monad m)=>(m a)->(m b)->(m a)
a << b = a >>= (\v->b >>= (\_->return v))

data Parser a = Parser (String -> [(String,a)])
parse::Parser a->String -> [(String,a)]
parse (Parser p) = p

instance Functor Parser where
  fmap f (Parser p) = Parser (\s -> [(s',f a)|(s',a)<-p s])

instance Applicative Parser where
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
p >>=? f = p >>= (\a-> (many . char) ' ' >> produce a >>= f)

(>>?)::(Parser a)->(Parser b)->(Parser b)
a >>? b = a >> (many . char) ' ' >> b

(<<?)::(Parser a)->(Parser b)->(Parser a)
a <<? b = a << (many . char) ' ' << b

produce::a->Parser a
produce x = Parser (\s->[(s,x)])

failure::Parser a
failure = Parser (\s->[])

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

many::Parser a->Parser [a]
many p = many1 p <|> return []

many1 p = p <:> many p

string::String -> Parser String
string [] = produce []
string (c:cs) = char c >>= (\a->string cs >>= (\a'->produce (c:cs)))

digit::Parser Char
digit = ( char '0' <|> char '1' <|> char '2' <|> char '3' <|> char '4' <|> char '5' <|> char '6' <|> char '7' <|> char '8' <|> char '9' )

any::[Parser a]->Parser a
any = foldr (<|>) failure

num::Parser (Fix Expr)
num = (In . Num . read) <$> (many1 digit)

type Defs = [[(String, Fix Expr->Fix Expr->Fix Expr)]]

subexpr::Defs->Parser (Fix Expr)
subexpr cs = char '(' >>? expr cs <<? char ')'

opParser::(String,a)->Parser a
opParser (s,a) = Parser (\s'-> case parse (string s) s' of
              [] -> []
              cs -> map (\(x,y)->(x,a)) cs)

halfexpr::Defs->Defs->Fix Expr->Parser (Fix Expr)
halfexpr [] _ ex = return ex
halfexpr (cs:css) alldefs ex = (do
                  sub <- halfexpr css alldefs ex
                  op <- Main.any (map opParser cs)
                  (many . char) ' '
                  a <- num <||> subexpr alldefs
                  a' <- halfexpr (cs:css) alldefs a
                  return (op sub a')
                )
                <||> halfexpr css alldefs ex

expr::Defs->Parser (Fix Expr)
expr cs = (do
          a <- num <||> subexpr cs
          (many . char) ' '
          a' <- halfexpr cs cs a
          return a'
         )

operators = [
              [
                ("+",(\a b->In (Plus a b))),
                ("-",(\a b->In (Subtract a b)))
              ],
              [
                ("*",(\a b->In (Times a b))),
                ("/",(\a b->In (Divide a b)))
              ]
            ]

expr' = (many . char) ' ' >> expr operators << (many . char) ' ' << eos

parseToExpr::Parser (Fix Expr)->String->Fix Expr
parseToExpr p = snd . head . (parse p)

parseAndEvalExpr::String->Int
parseAndEvalExpr = eval . (parseToExpr expr')

{-
<digit> := ( 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 )
<num> :=  <digit> <num> | <digit>
<expr> := <subexpr> <halfexpr>
<halfexpr> := 3 | '+' <subexpr> <halfexpr>
<subexpr> := '(' <expr> ')' | <num>
-}
