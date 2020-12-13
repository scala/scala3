type TestGood = String
val testGood = TestGood // error

type TestBad[A] = String
val testBad = TestBad[Nothing] // error
val testBadNoAp = TestBad // error

type TestBad2 = [A] =>> String
val testBad2 = TestBad2[Nothing] // error
val testBad2NoAp = TestBad2 // error

type TestBad3[A] = [B] =>> String
val testBad3NoAp = TestBad3 // error
val testBad3PartialAp = TestBad3[String] // error
val testBad3 = TestBad3[String][String] // error

type TestBad4[A, B] = [C, D] =>> [E] =>> String
val testBad4Ap0 = TestBad4 // error
val testBad4Ap1 = TestBad4[Nothing] // error
val testBad4Ap2 = TestBad4[Nothing, Nothing] // error
val testBad4Ap3 = TestBad4[Nothing, Nothing][Nothing] // error
val testBad4Ap4 = TestBad4[Nothing, Nothing][Nothing, Nothing] // error
val testBad4Ap5 = TestBad4[Nothing, Nothing][Nothing, Nothing][Nothing] // error

type Curried = [X] =>> [Y] =>> String
type PartialApplication[X] = Curried[String][X]
val test1 = PartialApplication[String] // error
val test2 = Curried[String][String] // error
