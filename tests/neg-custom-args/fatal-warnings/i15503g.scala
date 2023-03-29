// scalac: -Wunused:params

/* This goes around the "trivial method" detection */
val default_int = 1

def f1(a: Int) = a // OK
def f2(a: Int) = default_int // error
def f3(a: Int)(using Int) = a // error
def f4(a: Int)(using Int) = default_int // error // error
def f6(a: Int)(using Int) = summon[Int] // error
def f7(a: Int)(using Int) = summon[Int] + a // OK

/* --- Trivial method check --- */
def g1(x: Int) = 1 // OK
def g2(x: Int) = ??? // OK

package foo.test.i17101:
  type Test[A] = A
  extension[A] (x: Test[A]) { // OK
    def value: A = x
    def causesIssue: Unit = println("oh no")
  }
