import Test.HUnit
import Parsers

charTest1 = TestCase (assertEqual "" (parse (charParser 'a') "abc") (Success 'a' "bc"))
charTest2 = TestCase (assertEqual "" (parse (charParser 'b') "abc") Failure)
charTests = TestList [TestLabel "charParser test 1" charTest1, TestLabel "charParser test 2" charTest2]

eitherTest1 = TestCase (assertEqual ""
                        (parse (eitherCombinator (charParser 'a') (charParser 'b')) "abc")
                        (Success 'a' "bc"))

eitherTest2 = TestCase (assertEqual ""
                        (parse (eitherCombinator (charParser 'a') (charParser 'b')) "bce")
                        (Success 'b' "ce"))

eitherTest3 = TestCase (assertEqual ""
                        (parse (eitherCombinator (charParser 'a') (charParser 'b')) "fce")
                        Failure)

eitherTests = TestList [TestLabel "eitherCombinator test 1" eitherTest1,
                        TestLabel "eitherCombinator test 2" eitherTest2,
                        TestLabel "eitherCombinator test 3" eitherTest3]

integerTest1 = TestCase (assertEqual ""
                         (parse integerParser "123 hello")
                         (Success 123 " hello"))

integerTest2 = TestCase (assertEqual ""
                         (parse integerParser "hello 123")
                         Failure)

integerTests = TestList [TestLabel "integerParser test 1" integerTest1,
                         TestLabel "integerParser test 2" integerTest2]

tests = TestList [charTests, eitherTests, integerTests]
