LogiLang Parser
===============

Parser and reducer for the logic bit of the toy language from chapter 3 of Types and Programming Languages.
Will hopefully eveolve into something more complicated as I progress through the book and also as I become less incompetent at writing parsers.

To use, open up `ghci` and load `LogiLangParsersTests.hs`, since that includes everything else.

To just use to parser, do something like `parse termParser "(if true then false else true) and false"`.

To see a reduction, try `fmap reduce $ parse termParser "(if true then false else true) and false"`.

Full reduction: `fmap fullReduce $ parse termParser "(if true then false else true) and false"`.
