import deriving.Mirror

case class Cov[+T]()

class SubA[+T]() extends Cov[T]
class SubB[+T]() extends Cov[T]

val baz = summon[Mirror.Of[SubA[Int] | SubB[Int]]] // error
//        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//        this should fail because:
//        1) SubA and SubB are not individually product types
//        2) SubA and SubB are different classes
