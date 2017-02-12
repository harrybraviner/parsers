module Parsers where

data ParserResult a = Success a String | Failure deriving (Show, Eq)

data Parser a = Parser (String -> ParserResult a)

instance Functor ParserResult where
    fmap f Failure        = Failure
    fmap f (Success x xs) = Success (f x) xs

instance Functor Parser where
    fmap f (Parser p) =
        Parser(
            \stream -> case p stream of
                            Failure      -> Failure
                            Success x xs -> Success (f x) xs
        )

instance Applicative Parser where
    pure x =
        Parser(
            \stream -> Success x stream
        )
    (<*>) (Parser f) (Parser p) =
        Parser(
            \stream ->
                case f stream of
                    Failure      -> Failure
                    Success f xs -> case p xs of
                                        Failure      -> Failure
                                        Success x xs -> Success (f x) xs
        )
           
instance Monad Parser where
    (>>=) (Parser p) f =
        Parser(
            \stream ->
                case p stream of
                    Failure      -> Failure
                    Success x xs -> ff xs where Parser ff = f x
        )

--return :: a -> Parser a
--return x = Parser (\stream -> Success(x, stream))

parse :: Parser a -> String -> ParserResult a
parse (Parser f) s = f s

bind :: Parser a -> (a -> Parser b) -> Parser b
bind (Parser p1) f =
    Parser (\stream ->
        case p1 stream of
            Failure      -> Failure
            Success x xs -> ff xs where Parser ff = f x
    )

-- |If the stream starts with a c, returns Success, otherwise Failure
charParser :: Char -> Parser Char
charParser c =
    Parser (\stream ->
        case stream of
            x : xs -> if (x == c) then Success c xs
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

-- |Use the result of the first parser in the list that succeeds
anyCombinator :: [Parser a] -> Parser a
anyCombinator []     = Parser (\stream -> Failure)
anyCombinator [p]    = p
anyCombinator (p:ps) = eitherCombinator p (anyCombinator ps)

-- |Succeeds if the stream begins with one of the decimal digits
digitParser :: Parser Char
digitParser = foldl1 eitherCombinator $ map charParser ['0'..'9']

-- |Applies the parser as many times as possible
manyCombinator :: Parser a -> Parser ([a])
manyCombinator (Parser p) =
    Parser( \stream ->
        case p stream of
            Failure      -> Success [] stream
            Success x xs -> parse (fmap (\ys -> x : ys) (manyCombinator (Parser p))) xs
    )

-- |Applies the parser at least once, and as many times as possible thereafter
someCombinator :: Parser a -> Parser ([a])
someCombinator (Parser p) =
    Parser( \stream ->
        case p stream of
            Failure      -> Failure
            Success x xs -> parse (fmap (\ys -> x : ys) (manyCombinator (Parser p))) xs
    )

-- |Parses integers
integerParser :: Parser Integer
integerParser = fmap (read :: String -> Integer) (someCombinator digitParser)

-- |Parses strings
stringParser :: String -> Parser String
stringParser s =
    let pr :: Parser String -> Char -> Parser String
        pr (Parser p) c =
             Parser (\stream ->
                case p stream of
                    Failure      -> Failure
                    Success s xs -> q xs
                                       where q (y : ys) | y == c    = Success (s ++ [c]) ys
                                                        | otherwise = Failure
                                             q [] = Failure
             )
    in foldl pr (return "") s
