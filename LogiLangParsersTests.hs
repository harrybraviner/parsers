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

ifParserTest1 = TestCase (assertEqual ""
                            (Success (IfTerm (BooleanTerm LogiLang.True) (BooleanTerm LogiLang.False) (BooleanTerm LogiLang.True)) "")
                            (parse termParser "if true then false else true"))

ifParserTest2 = TestCase (assertEqual ""
                            (Success (IfTerm (BooleanTerm LogiLang.False) (BooleanTerm LogiLang.True) (BooleanTerm LogiLang.True)) "")
                            (parse termParser "if false then true else true"))

ifParserTest3 = TestCase (assertEqual ""
                            (Success (IfTerm (IfTerm (BooleanTerm LogiLang.False)
                                                     (BooleanTerm LogiLang.True )
                                                     (BooleanTerm LogiLang.False))
                                             (BooleanTerm LogiLang.True) (BooleanTerm LogiLang.True)) "")
                            (parse termParser "if if false then true else false then true else true"))

ifParserTest4 = TestCase (assertEqual ""
                            (Success (IfTerm (BooleanTerm LogiLang.False)
                                             (IfTerm (BooleanTerm LogiLang.False)
                                                     (BooleanTerm LogiLang.True )
                                                     (BooleanTerm LogiLang.False))
                                             (BooleanTerm LogiLang.True) ) "")
                            (parse termParser "if false then if false then true else false else true"))

ifParserTest5 = TestCase (assertEqual ""
                            (Success (IfTerm (BooleanTerm LogiLang.True)
                                             (BooleanTerm LogiLang.False)
                                             (IfTerm (BooleanTerm LogiLang.False)
                                                     (BooleanTerm LogiLang.True )
                                                     (BooleanTerm LogiLang.False)) ) "")
                            (parse termParser "if true then false else if false then true else false"))


ifParserTests = TestList [TestLabel "ifParser tft test" ifParserTest1,
                          TestLabel "ifParser ftt test" ifParserTest2,
                          TestLabel "ifParser (if)tt test" ifParserTest3,
                          TestLabel "ifParser f(if)t test" ifParserTest4,
                          TestLabel "ifParser tf(if) test" ifParserTest5]

andParserTest1 = TestCase (assertEqual ""
                           (Success (AndTerm (BooleanTerm LogiLang.True)
                                             (BooleanTerm LogiLang.True)) "")
                           (parse termParser "true and true"))

andParserTest2 = TestCase (assertEqual ""
                           (Success (AndTerm (BooleanTerm LogiLang.False)
                                             (BooleanTerm LogiLang.True)) "")
                           (parse termParser "false and true"))

andParserTest3 = TestCase (assertEqual ""
                           (Success (IfTerm (BooleanTerm LogiLang.False)
                                            (BooleanTerm LogiLang.True )
                                            (AndTerm (BooleanTerm LogiLang.False)
                                                     (BooleanTerm LogiLang.False))) "")
                           (parse termParser "if false then true else false and false"))

andParserTest4 = TestCase (assertEqual ""
                           (Success (AndTerm (IfTerm (BooleanTerm LogiLang.False)
                                                     (BooleanTerm LogiLang.True )
                                                     (BooleanTerm LogiLang.False))
                                    (BooleanTerm LogiLang.False)) "")
                           (parse termParser "(if false then true else false) and false"))

andParserTests = TestList [TestLabel "andParser tt test" andParserTest1,
                           TestLabel "andParser ft test" andParserTest2,
                           TestLabel "andParser (if)f test" andParserTest3,
                           TestLabel "andParser (if)(and) test" andParserTest4]

tests = TestList [boolParserTests, ifParserTests, andParserTests]
