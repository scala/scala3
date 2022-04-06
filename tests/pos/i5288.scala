trait C { type M; val m: M }

object Test {
// Arity 1 ------------------------------------------------------------------

// Function1
def m1(i: Int): Int = 1
val f1Expected: Int => Int = m1
val f1Inferred = m1
identity[Int => Int](f1Inferred)

// ImplicitFunction1
def m4(using i: Int): Int = 4
val f4Expected: Int ?=> Int = m4
// val f4Inferred = m4  // can't work since no expected type
// identity[Int ?=> Int](f4Inferred)

// DependentFunction1
def m7(c: C): c.M = c.m
val f7Expected: (c: C) => c.M = m7
val f7Inferred = m7
identity[(c: C) => c.M](f7Inferred)

// Arity 2 ------------------------------------------------------------------

// Function2
def m2(i: Int, s: String): Int = 2
val f2Expected: (Int, String) => Int = m2
val f2Inferred = m2
identity[(Int, String) => Int](f2Inferred)

// ImplicitFunction2
def m5(using i: Int, s: String): Int = 5
val f5Expected: (Int, String) ?=> Int = m5
// val f5Inferred = m5 // can't work since no expected type
// identity[(Int, String) ?=> Int](f5Inferred)

// DependentFunction2
def m9(c1: C, c2: C): c1.M | c2.M = c1.m
val f9Expected: (c1: C, c2: C) => c1.M | c2.M = m9
val f9Inferred = m9
identity[(c1: C, c2: C) => c1.M | c2.M](f9Inferred)

// Function1[Function1]
def m8(i: Int)(s: String): Int = 8
val f8Expected: Int => String => Int = m8
val f8Inferred = m8
identity[Int => String => Int](f8Inferred)

// Function1[ImplicitFunction1]
def m6(i: Int)(using s: String): Int = 6
val f6Expected: Int => String ?=> Int = m6
//val f6Inferred = m6 // can't work since no expected type
//identity[Int => String ?=> Int](f6Inferred)

// Function1[DependentFunction1]
def mA(i: Int)(c: C): c.M = c.m
val fAExpected: Int => (c: C) => c.M = mA
val fAInferred = mA
identity[Int => (c: C) => c.M](fAInferred)

// ImplicitFunction1[ImplicitFunction1]  -- Can't be expressed as a method...
// ImplicitFunction1[Function1]          -- Can't be expressed as a method...
// ImplicitFunction1[DependentFunction1] -- Can't be expressed as a method...

// DependentFunction1[Function1]
def mB(c: C)(s: String): c.M = c.m
val fBExpected: (c: C) => String => c.M = mB
val fBInferred = mB
identity[(c: C) => String => c.M](fBInferred)

// DependentFunction1[ImplicitFunction1]
def mC(c: C)(using s: String): c.M = c.m
// val fCExpected: (c: C) => String ?=> c.M = mC
    // gives:
    // Implementation restriction: Expected result type (c: C) => (String) ?=> c.M
    // is a curried dependent context function type. Such types are not yet supported.

// val fCInferred = mC  // can't work since no expected type
// identity[(c: C) => String ?=> c.m](fCInferred)

// DependentFunction1[DependentFunction1]
def mD(c1: C)(c2: C): c1.M | c2.M = c1.m
val fDExpected: (c1: C) => (c2: C) => c1.M | c2.M = mD
val fDInferred = mD
identity[(c1: C) => (c2: C) => c1.M | c2.M](fDInferred)

// Missing from the above:
// - interactions with by name
// - interactions with default arguments
// - interactions with inline method
// - interactions with inline arguments
}