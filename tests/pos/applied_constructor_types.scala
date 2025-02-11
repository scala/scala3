import scala.language.experimental.modularity

class Box(tracked val v: Any)
class C(tracked val x: Int)
class NC(tracked val c: C)
class NNC(tracked val c: NC)
class F[A](tracked val a: Int)
class G[A](tracked val a: A)
class NF[A](tracked val f: F[A])

class Person(val name: String, tracked val age: Int)
class PersonPrime(val name: String)(tracked val age: Int)
class PersonBis(tracked val name: String)(val age: Int)

class Generic[A](val a: A)

object O:
  val m: Int = 27

  class InnerClass(tracked val x: Int)

object Test extends App {
  val c: C(42) = C(42)
  val nc: NC(C(42)) = NC(C(42))
  val nc1: NC(c) = NC(c)
  val nnc: NNC(NC(C(42))) = NNC(NC(C(42)))
  val f: F[Int](42) = F[Int](42)
  val f2: F[Int](42) = F(42)
  val f3: F(42) = F(42)
  val g: G(42) = G(42)

  val n: Int = 27
  val c2: C(n) = C(n)
  val c3: C(O.m) = C(O.m)

  val box: Box(O.InnerClass(42)) = Box(O.InnerClass(42))
  val box2: Box(O.InnerClass(n)) = Box(O.InnerClass(n))
  val box3: Box(O.InnerClass(O.m)) = Box(O.InnerClass(O.m))

  val person: Person("Kasia", 27) = Person("Kasia", 27) // warn
  val person1: Person("Kasia", n) = Person("Kasia", n) // warn
  val person2: Person("Kasia", O.m) = Person("Kasia", O.m) // warn

  val personPrime: PersonPrime("Kasia")(27) = PersonPrime("Kasia")(27) // warn
  val personPrime1: PersonPrime("Kasia")(n) = PersonPrime("Kasia")(n) // warn
  val personPrime2: PersonPrime("Kasia")(O.m) = PersonPrime("Kasia")(O.m) // warn

  val personBis: PersonBis("Kasia")(27) = PersonBis("Kasia")(27) // warn
  val personBis1: PersonBis("Kasia")(n) = PersonBis("Kasia")(n) // warn
  val personBis2: PersonBis("Kasia")(O.m) = PersonBis("Kasia")(O.m) // warn

  val generic1: Generic(compiletime.erasedValue[Int]) = Generic(42) // warn
  val generic2: Generic(??? : Int) = Generic(42) // warn
  val generic3: Generic(43) = Generic(42) // warn
}
