import Test.HUnit
import LogiLang

valueTest1 = TestCase (assertEqual "" (isValue (BooleanTerm LogiLang.True)) Prelude.True)
valueTest2 = TestCase (assertEqual "" (isValue (BooleanTerm LogiLang.False)) Prelude.True)
valueTest3 = TestCase (assertEqual "" (isValue (IfTerm (BooleanTerm LogiLang.True)
                                                       (BooleanTerm LogiLang.True)
                                                       (BooleanTerm LogiLang.False))) Prelude.False)

valueTests = TestList [TestLabel "isValue test on True"    valueTest1,
                       TestLabel "isValue test on False"   valueTest2,
                       TestLabel "isValue test on if term" valueTest3]

reductionTest1 = TestCase (assertEqual ""
                                (reduce (IfTerm (BooleanTerm LogiLang.True)
                                                (BooleanTerm LogiLang.False)
                                                (BooleanTerm LogiLang.True)))
                                (BooleanTerm LogiLang.False))
reductionTest2 = TestCase (assertEqual ""
                                (reduce (IfTerm (BooleanTerm LogiLang.False)
                                                (BooleanTerm LogiLang.False)
                                                (BooleanTerm LogiLang.True)))
                                (BooleanTerm LogiLang.True))
reductionTest3 = TestCase (assertEqual ""
                                (reduce (IfTerm (IfTerm (BooleanTerm LogiLang.False)
                                                        (BooleanTerm LogiLang.True )
                                                        (BooleanTerm LogiLang.False))
                                                (BooleanTerm LogiLang.False)
                                                (BooleanTerm LogiLang.True)))
                                (IfTerm (BooleanTerm LogiLang.False)
                                        (BooleanTerm LogiLang.False)
                                        (BooleanTerm LogiLang.True )))
reductionTest4 = TestCase (assertEqual "" 
                                (BooleanTerm LogiLang.True)
                                (reduce (AndTerm (BooleanTerm LogiLang.True) (BooleanTerm LogiLang.True))))
reductionTest5 = TestCase (assertEqual "" 
                                (BooleanTerm LogiLang.False)
                                (reduce (AndTerm (BooleanTerm LogiLang.False) (BooleanTerm LogiLang.True))))
reductionTest6 = TestCase (assertEqual "" 
                                (BooleanTerm LogiLang.False)
                                (reduce (AndTerm (BooleanTerm LogiLang.True) (BooleanTerm LogiLang.False))))
reductionTest7 = TestCase (assertEqual "" 
                                (BooleanTerm LogiLang.False)
                                (reduce (AndTerm (BooleanTerm LogiLang.False) (BooleanTerm LogiLang.False))))
reductionTest8 = TestCase (assertEqual "" 
                                (AndTerm (BooleanTerm LogiLang.False) (BooleanTerm LogiLang.False))
                                (reduce (AndTerm (AndTerm (BooleanTerm LogiLang.False)
                                                          (BooleanTerm LogiLang.True))
                                        (BooleanTerm LogiLang.False))))
reductionTest9 = TestCase (assertEqual "" 
                                (AndTerm (BooleanTerm LogiLang.True) (BooleanTerm LogiLang.True))
                                (reduce (AndTerm (BooleanTerm LogiLang.True)
                                                 (AndTerm (BooleanTerm LogiLang.True)
                                                          (BooleanTerm LogiLang.True)))))


reductionTests = TestList [TestLabel "IfTrue reduction"       reductionTest1,
                           TestLabel "IfFalse reduction"      reductionTest2,
                           TestLabel "IfTerm reduction"       reductionTest3,
                           TestLabel "AndTerm T T reduction"  reductionTest4,
                           TestLabel "AndTerm F T reduction"  reductionTest5,
                           TestLabel "AndTerm T F reduction"  reductionTest6,
                           TestLabel "AndTerm F F reduction"  reductionTest7,
                           TestLabel "AndTerm t F reduction"  reductionTest8,
                           TestLabel "AndTerm T t reduction"  reductionTest9]

fullyReduceTest1 = TestCase (assertEqual ""
                                (fullyReduce (IfTerm (BooleanTerm LogiLang.True)
                                                     (BooleanTerm LogiLang.False)
                                                     (BooleanTerm LogiLang.True)))
                                (BooleanTerm LogiLang.False))
fullyReduceTest2 = TestCase (assertEqual ""
                                (fullyReduce (BooleanTerm LogiLang.True))
                                (BooleanTerm LogiLang.True))
fullyReduceTest3 = TestCase (assertEqual ""
                                (fullyReduce (IfTerm (IfTerm (BooleanTerm LogiLang.False)
                                                             (BooleanTerm LogiLang.True )
                                                             (BooleanTerm LogiLang.False))
                                                     (BooleanTerm LogiLang.False)
                                                     (BooleanTerm LogiLang.True)))
                                (BooleanTerm LogiLang.True))
fullyReduceTest4 = TestCase (assertEqual ""
                                (BooleanTerm LogiLang.True)
                                (fullyReduce (AndTerm (IfTerm (BooleanTerm LogiLang.False)
                                                              (BooleanTerm LogiLang.False)
                                                              (BooleanTerm LogiLang.True ))
                                                      (IfTerm (BooleanTerm LogiLang.True )
                                                              (BooleanTerm LogiLang.True )
                                                              (BooleanTerm LogiLang.True )))))

fullyReduceTests = TestList [TestLabel "IfTerm reduction"         fullyReduceTest1,
                             TestLabel "value reduction"          fullyReduceTest2,
                             TestLabel "IfTerm double reduction"  fullyReduceTest3,
                             TestLabel "AndTerm double reduction" fullyReduceTest4]

isZeroTest1 = TestCase (assertEqual ""
                            (BooleanTerm LogiLang.True)
                            (reduce (IsZero Zero)))

isZeroTest2 = TestCase (assertEqual ""
                            (BooleanTerm LogiLang.False)
                            (reduce (IsZero (Succ Zero))))

isZeroTest3 = TestCase (assertEqual ""
                            (BooleanTerm LogiLang.False)
                            (reduce (IsZero (Succ (Pred Zero)))))

isZeroTests = TestList [TestLabel "isZero Zero" isZeroTest1,
                        TestLabel "isZero 1"    isZeroTest2,
                        TestLabel "isZero succ pred zero" isZeroTest3]

predTest1 = TestCase (assertEqual ""
                        Zero
                        (reduce (Pred Zero)))

predTest2 = TestCase (assertEqual ""
                        (Succ Zero)
                        (reduce (Pred (Succ (Succ Zero)))))

predTests = TestList [TestLabel "Pred Zero" predTest1,
                      TestLabel "Pred Succ Zero" predTest2]

succTest1 = TestCase (assertEqual ""
                        (Succ Zero)
                        (reduce (Succ (Pred Zero))))

succTest2 = TestCase (assertEqual ""
                        (Succ Zero)
                        (reduce (Succ (Pred (Succ Zero)))))

succTests = TestList [TestLabel "Succ Pred Zero" succTest1,
                      TestLabel "Succ Pred Succ Zero" succTest2]

tests = TestList [valueTests, reductionTests, fullyReduceTests, isZeroTests, predTests, succTests]
