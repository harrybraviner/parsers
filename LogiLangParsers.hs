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

termParser :: Parser Term
termParser =
    eitherCombinator boolParser ifParser 
