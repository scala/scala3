// All three All type positions in a match type (scrutinee, patterns, bodies)
// are considered invariant, as showed by the following examples:

trait TA[+Plus] { type M[X] = Plus match { case Int => String } } // error
//               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//               covariant type Plus occurs in invariant position in type [X] =
//                    Plus match {
//                      case Int => String
//                    } of type M

trait TB[+Plus] { type M[X] = X match { case Plus => String } } // error
//               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//               covariant type Plus occurs in invariant position in type [X] =
//                 X match {
//                   case Plus => String
//                 } of type M

trait TC[+Plus] { type M[X] = X match { case Int => Plus } } // error
//               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//               covariant type Plus occurs in invariant position in type [X] =
//                 X match {
//                   case Int => Plus
//                 } of type M

trait TD[-Minus] { type M[X] = Minus match { case Int => String } } // error
//                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//                contravariant type Minus occurs in invariant position in type [X] =
//                  Minus match {
//                    case Int => String
//                  } of type M

trait TE[-Minus] { type M[X] = X match { case Minus => String } } // error
//                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//                contravariant type Minus occurs in invariant position in type [X] =
//                  X match {
//                    case Minus => String
//                  } of type M

trait TF[-Minus] { type M[X] = X match { case Int => Minus } } // error
//                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//                contravariant type Minus occurs in invariant position in type [X] =
//                  X match {
//                    case Int => Minus
//                  } of type M

// Furthermore, both unannotated type parameters and unannotated type bindings
// in patterns are invariant, as showed by the following examples:

trait Cov[+X]
trait Contra[-X]

// As usual:
type Test0[X] = Cov[X]     // OK
type Test1[X] = Contra[X]  // OK
type Test2[+X] = Contra[X] // error: covariant type parameter X occurs in
                           // in contravariant position in Contra[X]
type Test3[-X] = Cov[X]    // error: contravariant type parameter X occurs in
                           // covariant position in Cov[X]

type M0[X] = X match { case Int => Cov[X] }
type M1[X] = X match { case Int => Contra[X] }
type M2[X] = X match { case Cov[x] => Contra[x] }
type M3[X] = X match { case Contra[x] => Cov[x] }
