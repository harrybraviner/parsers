module LogiLangParsers where

import Parsers
import LogiLang

trueParser :: Parser Boolean
trueParser =
    fmap (\_ -> LogiLang.True) (stringParser "true")
    
falseParser :: Parser Boolean
falseParser =
    fmap (\_ -> LogiLang.False) (stringParser "false")
    
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

zeroParser :: Parser Term
zeroParser =
    fmap (\_ -> Zero) (stringParser "0")

succParser :: Parser Term
succParser =
    fmap (\_ -> (\x -> Succ x)) (stringParser "succ")
    <*> Parser (\stream -> parse termParser stream)

predParser :: Parser Term
predParser =
    fmap (\_ -> (\x -> Pred x)) (stringParser "pred")
    <*> Parser (\stream -> parse termParser stream)

isZeroParser :: Parser Term
isZeroParser =
    fmap (\_ -> (\x -> IsZero x)) (stringParser "iszero")
    <*> Parser (\stream -> parse termParser stream)

andParser :: Parser Term
andParser =
    fmap (\firstClause -> AndTerm firstClause) (Parser (\stream -> parse nonAndParser stream))
    <* whitespaceParser
    <* stringParser "and"
    <* whitespaceParser
    <*> Parser (\stream -> parse termParser stream)

bracketedTermParser :: Parser Term
bracketedTermParser =
    charParser '('
    *> Parser (\stream -> parse termParser stream)
    <* manyCombinator whitespaceParser
    <* charParser ')'

leadingPaddedTermParser :: Parser Term
leadingPaddedTermParser =
    (someCombinator whitespaceParser)
    *> Parser (\stream -> parse termParser stream)

nonAndParser :: Parser Term
nonAndParser =
    anyCombinator [boolParser,
                   ifParser,
                   zeroParser,
                   succParser,
                   predParser,
                   isZeroParser,
                   bracketedTermParser,
                   leadingPaddedTermParser]

termParser :: Parser Term
termParser =
    anyCombinator [andParser,
                   boolParser,
                   ifParser,
                   zeroParser,
                   succParser,
                   predParser,
                   isZeroParser,
                   bracketedTermParser,
                   leadingPaddedTermParser]
