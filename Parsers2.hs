data ParserResult a = Success(a, String) | Failure deriving Show

type Parser a = String -> ParserResult a

instance Functor ParserResult where
    fmap f Failure          = Failure
    fmap f (Success(x, xs)) = Success(f x, xs)

return :: a -> Parser a
return x = \stream -> Success(x, stream)

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p1 f =
    \stream ->
        case p1 stream of
            Failure        -> Failure
            Success(x, xs) -> (f x) xs

-- |If the stream starts with a c, returns Success, otherwise Failure
charParser :: Char -> Parser Char
charParser c =
    \stream ->
        case stream of
            x : xs -> if (x == c) then Success(c, xs)
                                  else Failure
            _      -> Failure

-- |If the first parser succeeds, use its result, otherwise use the second parser
eitherCombinator :: Parser a -> Parser a -> Parser a
eitherCombinator p1 p2 =
    \stream ->
        case p1 stream of
            Failure -> p2 stream
            res     -> res

-- |Succeeds if the stream begins with one of the decimal digits
digitParser :: Parser Char
digitParser = foldl1 eitherCombinator $ map charParser ['0'..'9']

-- |Applies f to the result of the parser, if it succeeds
applyCombinator :: Parser a -> (a -> b) -> Parser b
applyCombinator p f =
    \stream ->
        case p stream of
            Failure        -> Failure
            Success(x, xs) -> Success(f x, xs)

-- |Applies the parser as many times as possible
manyCombinator :: Parser a -> Parser ([a])
manyCombinator p =
    \stream ->
        case p stream of
            Failure        -> Success([], stream)
            Success(x, xs) -> applyCombinator (manyCombinator p) (\ys -> x : ys) xs

-- |Parses integers
integerParser :: Parser Integer
integerParser = applyCombinator (manyCombinator digitParser) (read :: String -> Integer)    -- can I put some error handling in here?
