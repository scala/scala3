// Draft of higher order expressions
// ---------------------------------
// A higher order expression is a an expression that may contain unbound variables.
// Unbound variables are listed as arguments of the type lambda of the expression.
// Splicing can only be done on Expr[_ <: Any].
// `apply methods on higher order expression provide a way to replace the unbound variables.

// This can be used for HOAS quoted patterns to not return a lambda (see HOAS patterns section in https://infoscience.epfl.ch/record/288718?ln=en).
// The use would be the same as each higher order expression would have an apply method.
// But as it would be an expression, the expression would showable.
// The expression could also be directly transformed into a Term of the reflection API.

// Question: How to extend this to allow unbound type (Type) in the expression?

class Expr[+T <: AnyKind]
object Expr:
  extension [T, R](hoe: Expr[[_ >: T <: T] =>> R]) def apply(x1: Expr[T]): Expr[R] = ???
  extension [T1, T2, R](hoe: Expr[[_ >: T1 <: T1, _ >: T2 <: T2] =>> R]) def apply(x1: Expr[T1], x2: Expr[T2]): Expr[R] = ???
  // Are lower bounds in lambda parameters necessary?
  // How could this be generalized to n arguments?


def `'`[T](e: T): Expr[T] = ???
def `$`[T](e: Expr[T]): T = ???

def f(): Unit =
  val e: Expr[Int] = ???
  val hoe1: Expr[[T >: Int <: Int] =>> Int] = ??? // assumed to come from HOAS pattern with one unbound variable of type Int
  val hoe2: Expr[[T1 >: Int <: Int, T2 >: Int <: Int] =>> Int] = ??? // assumed to come from HOAS pattern with 2 unbound variables of type Int
  val e2: Expr[Int] = hoe1(e)
  val e3: Expr[Int] = hoe2(e, e)

  {
    `$`{e}
    `$`{e2}
    `$`{e3}
    `$`{hoe1(e)}
    `$`{hoe2(e, e)}
    `$`{hoe1} // error: Found: Expr[[T >: Int <: Int] =>> Int]), Required: Expr[Any] // may contain references to 1 unbound variable
    `$`{hoe2} // error // may contain references to 2 unbound variables
  }
  `'`{1}
  `'`{`$`{e}}
  `'`{??? : ([T >: Int <: Int] =>> Int)} // error: Missing type parameter for [T >: Int <: Int] =>> Int // not a valid quote literal expression
