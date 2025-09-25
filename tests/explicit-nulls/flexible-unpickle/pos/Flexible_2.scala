import unsafeNulls.Foo.*
import unsafeNulls.Unsafe_1
import unsafeNulls.{A, B, C, F, G, H, I, J, L, M, N, S, T, U, expects}
import unsafeNulls.ZIO
import scala.reflect.Selectable.reflectiveSelectable
import scala.quoted.*

class Inherit_1 extends Unsafe_1 {
  override def foo(s: String): String = s
  override def bar[T >: String](s: T): T = s
  override def bar2[T >: String | Null](s: T): T = s
  override def bar3[T <: Function1[String,String]](g: T) = g
  override def bar4[HK[_]](i: String | Null): HK[String | Null] = ???
}

class Inherit_2 extends Unsafe_1 {
  override def foo(s: String | Null): String | Null = null
  override def bar[T >: String](s: T | Null): T | Null = s
  override def bar2[T >: String](s: T): T = s
  override def bar3[T <: Function1[(String|Null),(String|Null)]](g: T) = g
  override def bar4[HK[_]](i: String): HK[String] = ???
}

class Inherit_3 extends Unsafe_1 {
  override def foo(s: String): String | Null = null
  override def bar[T >: String](s: T): T | Null = s
}

class Inherit_4 extends Unsafe_1 {
  override def foo(s: String | Null): String = "non-null string"
  override def bar[T >: String](s: T | Null): T = "non-null string"
}

case class cc()

class K(val b: String) extends J(b) {
}

def typeNameMacro[A: Type](using Quotes) = Expr(Type.show[A])

@main
def Flexible_2() =
  val s2: String | Null = "foo"
  val unsafe = new Unsafe_1()
  val s: String = unsafe.foo(s2)
  unsafe.foo("")
  unsafe.foo(null)


  val a = refinement.b
  refinement.b = null
  val refinement2: Unsafe_1 { var b: String } = refinement
  refinement = null

  val singletonbar: bar.type = singleton

  val extension: String = intersection.reverse

  val stringA: String = intersection.stringA
  val stringB: String = intersection.stringB
  intersection.stringA = null
  intersection.stringB = null

  val intersection2: A & B = intersection
  intersection = null

  val stringC: String = union.stringC
  union.stringC = null

  val union2: A | B = union
  union = null

  val constructorTest = new Unsafe_1(null)
  val member: String = constructorTest.member
  constructorTest.member = null

  bar match {
    case str @ null: String => ()
    case other => ()
  }

  val f = new F(null, G(12))
  val F(x, y) = f

  val g: (List[F] | String | List[Int]) = F.many
  F.many = null :: null :: Nil
  F.many = null

  val h: H { val s: String } = new H { override val s: String = "foo" }

  val jBox: I[J] = new I(new J(null))
  val kBox: I[K] = new I(new K("foo"))

  val box: I[J] = kBox

  val jBox2: L[J] = new L[J](j => ())
  val kBox2: L[K] = new L[K](k => ())

  val box2: L[K] = jBox2
  val box3: I[J | Null] = box

  val m: String = M.test(null)

  // i23911
  val n1: List[Map[String, Int]] = ???
  val n2 = new N[List]()
  val n3 = n2.accept[Any](n1)

  // i23845
  transparent inline def typeName[A]: String = ${typeNameMacro[A]}

  implicit val givenT: T = ???
  def alphaTypeNameMacro[A: S](using T) = U(S.show[A])
  def res[A] = {
    implicit val givenS: S[A] = ???
    expects(alphaTypeNameMacro[A])
  }

// i23935
opaque type ZArrow[-I, -R, +E, +O] = I => ZIO[R, E, O]
object ZArrow:
  def fromZIOAttempt[I, R, E, O](f: I => ZIO[R, E, O]): ZArrow[I, R, Throwable | E, O] =
    (in: I) => ZIO.attempt(f(in)).flatten