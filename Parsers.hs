data ParserResult a = Success(a, String) | Failure deriving Show

data Parser a = Parser (String -> ParserResult a)

instance Functor ParserResult where
    fmap f Failure          = Failure
    fmap f (Success(x, xs)) = Success(f x, xs)

instance Functor Parser where
    fmap f (Parser p) =
        Parser(
            \stream -> case p stream of
                            Failure -> Failure
                            Success(x, xs) -> Success(f x, xs)
        )

instance Applicative Parser where
    pure x =
        Parser(
            \stream -> Success(x, stream)
        )
    (<*>) (Parser f) (Parser p) =
        Parser(
            \stream ->
                case f stream of
                    Failure        -> Failure
                    Success(f, xs) -> case p xs of
                                        Failure        -> Failure
                                        Success(x, xs) -> Success(f x, xs)
        )
           
instance Monad Parser where
    (>>=) (Parser p) f =
        Parser(
            \stream ->
                case p stream of
                    Failure        -> Failure
                    Success(x, xs) -> ff xs where Parser ff = f x
        )

--return :: a -> Parser a
--return x = Parser (\stream -> Success(x, stream))

parse :: Parser a -> String -> ParserResult a
parse (Parser f) s = f s

bind :: Parser a -> (a -> Parser b) -> Parser b
bind (Parser p1) f =
    Parser (\stream ->
        case p1 stream of
            Failure        -> Failure
            Success(x, xs) -> ff xs where Parser ff = f x
    )

-- |If the stream starts with a c, returns Success, otherwise Failure
charParser :: Char -> Parser Char
charParser c =
    Parser (\stream ->
        case stream of
            x : xs -> if (x == c) then Success(c, xs)
                                  else Failure
            _      -> Failure
    )

-- |If the first parser succeeds, use its result, otherwise use the second parser
eitherCombinator :: Parser a -> Parser a -> Parser a
eitherCombinator (Parser p1) (Parser p2) =
    Parser (\stream ->
        case p1 stream of
            Failure -> p2 stream
            res     -> res
    )

-- |Succeeds if the stream begins with one of the decimal digits
digitParser :: Parser Char
digitParser = foldl1 eitherCombinator $ map charParser ['0'..'9']

-- |Applies f to the result of the parser, if it succeeds
applyCombinator :: Parser a -> (a -> b) -> Parser b
applyCombinator (Parser p) f =
    Parser (\stream ->
        case p stream of
            Failure        -> Failure
            Success(x, xs) -> Success(f x, xs)
    )

-- |Applies the parser as many times as possible
manyCombinator :: Parser a -> Parser ([a])
manyCombinator (Parser p) =
    Parser( \stream ->
        case p stream of
            Failure        -> Success([], stream)
            Success(x, xs) -> pp xs
                              where (Parser pp) = applyCombinator (manyCombinator (Parser p)) (\ys -> x : ys)
    )

-- |Applies the parser at least once, and as many times as possible thereafter
someCombinator :: Parser a -> Parser ([a])
someCombinator (Parser p) =
    Parser( \stream ->
        case p stream of
            Failure        -> Failure
            Success(x, xs) -> pp xs
                              where (Parser pp) = applyCombinator (manyCombinator (Parser p)) (\ys -> x : ys)
    )

-- |Parses integers
integerParser :: Parser Integer
integerParser = applyCombinator (manyCombinator digitParser) (read :: String -> Integer)    -- can I put some error handling in here?

-- |Parses strings
stringParser :: String -> Parser String
stringParser s =
    let pr :: Parser String -> Char -> Parser String
        pr (Parser p) c =
             Parser (\stream ->
                case p stream of
                    Failure         -> Failure
                    Success (s, xs) -> q xs
                                       where q (y : ys) | y == c    = Success(s ++ [c], ys)
                                                        | otherwise = Failure
                                             q [] = Failure
             )
    in foldl pr (return "") s

-- Below here are specialized parsers for implementing our toy language

data Boolean = True | False deriving Show
data Term = BooleanTerm Boolean | IfTerm Term Term Term deriving Show

trueParser :: Parser Boolean
trueParser =
    fmap (\_ -> Main.True) (stringParser "true")
    
falseParser :: Parser Boolean
falseParser =
    fmap (\_ -> Main.False) (stringParser "false")
    
boolParser :: Parser Term
boolParser =
    eitherCombinator (fmap BooleanTerm trueParser) (fmap BooleanTerm falseParser)

whitespaceParser :: Parser ()
whitespaceParser =
    fmap (\_ -> ()) $ someCombinator $ eitherCombinator (charParser '\t') (charParser ' ')

ifParser :: Parser Term
ifParser =
    -- Note: The funny little Parser (\stream -> parse termParser stream) construct below
    --       seems to be necessary to avoid an infinite loop.
    --       I think the loop happens on construction when it tries to evaluate that the
    --       termParser actually is, before the stream starts being passed through it.
    --       To be honest, I don't fully understand it, but this belayed evaluation seems
    --       to solve the problem.
    let ifClause =
            stringParser "if"
            *> whitespaceParser
            *> Parser (\stream -> parse termParser stream)
            <* whitespaceParser
        thenClause =
            stringParser "then"
            *> whitespaceParser
            *> Parser (\stream -> parse termParser stream)
            <* whitespaceParser
        elseClause =
            stringParser "else"
            *> whitespaceParser
            *> Parser (\stream -> parse termParser stream)
    in fmap (\ifClause -> (\thenClause -> (\elseClause -> IfTerm ifClause thenClause elseClause))) ifClause
        <*> thenClause
        <*> elseClause

termParser :: Parser Term
termParser =
    eitherCombinator boolParser ifParser 
