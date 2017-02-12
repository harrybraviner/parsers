import Test.HUnit
import LogiLangParsers
import Parsers
import LogiLang

boolParserTest1 = TestCase (assertEqual ""
                            (Success (BooleanTerm LogiLang.True) "")
                            (parse boolParser "true"))
boolParserTest2 = TestCase (assertEqual ""
                            (Success (BooleanTerm LogiLang.False) "")
                            (parse boolParser "false"))

boolParserTests = TestList [TestLabel "boolParser true test"  boolParserTest1,
                            TestLabel "boolParser false test" boolParserTest2]

tests = TestList [boolParserTests]
