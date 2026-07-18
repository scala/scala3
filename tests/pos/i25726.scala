import scala.language.experimental.erasedDefinitions

class Ev

trait Svc[A]:
  def run(using erased x: Ev)(a: A): String
class ISvc extends Svc[Int]:
  def run(using erased x: Ev)(a: Int): String = a.toString

// More than one pre-erasure parameter list with an all-erased prefix
class Token
trait Store[A]:
  def put(using erased t: Token)(key: String)(value: A): String
class IntStore extends Store[Int]:
  def put(using erased t: Token)(key: String)(value: Int): String = s"$key=$value"

// Trailing all-erased list (worked before the fix; must not regress)
trait Trail[A]:
  def id(a: A)(using erased e: Ev): A
class IntTrail extends Trail[Int]:
  def id(a: Int)(using erased e: Ev): Int = a

// Erased-first inside a single parameter list
trait Single[A]:
  def run(erased a: Ev, b: A): String
class IntSingle extends Single[Int]:
  def run(erased a: Ev, b: Int): String = b.toString

// Erased-first within a non-trailing parameter list
trait Mixed[A]:
  def run(erased e: Ev, a: A)(b: Int): String
class IntMixed extends Mixed[Int]:
  def run(erased e: Ev, a: Int)(b: Int): String = s"$a+$b"

  trait Eta[A]:
    def f(using erased e: Ev)(a: A): String ?=> Int

  class IntEta extends Eta[Int]:
    def f(using erased e: Ev)(a: Int): String ?=> Int = a + summon[String].length

// generated bridge:
// def f(a: Object): Function1[String, Int] = // beridge
//   new Function1[String, Int]:
//     def apply(s: String): Int = this$IntEta.f(a, s)
trait Closure[A]:
  def f(using erased e: Ev)(a: A): String ?=> Int
class IntClosure extends Closure[Int]:
  def f(using erased e: Ev)(a: Int): String ?=> Int = a + summon[String].length
