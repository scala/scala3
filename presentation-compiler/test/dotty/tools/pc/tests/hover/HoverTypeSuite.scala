package dotty.tools.pc.tests.hover

import dotty.tools.pc.base.BaseHoverSuite

import org.junit.Test

class HoverTypeSuite extends BaseHoverSuite:

  @Test def `union` =
    check(
      """
        |import java.nio.file._
        |case class Foo(x: Int)
        |case class Bar[T](x: T)
        |object a {
        |  val name: Foo | Bar[Files] = Foo(1)
        |  <<na@@me>>
        |}
        |""".stripMargin,
      """|val name: Foo | Bar[Files]
         |""".stripMargin.hover
    )

  @Test def `intersection` =
    check(
      """
        |import java.nio.file._
        |
        |trait Resettable:
        |  def reset(): Unit
        |
        |trait Growable[T]:
        |  def add(t: T): Unit
        |
        |def f(arg: Resettable & Growable[Files]) = {
        |  <<ar@@g.reset()>>
        |}
        |""".stripMargin,
      """|arg: Resettable & Growable[Files]
         |""".stripMargin.hover
    )

  // We should produce a shorter type but:
  // https://github.com/scala/scala3/issues/11683
  @Test def `enums` =
    check(
      """|
         |object SimpleEnum:
         |  enum Color:
         |   case <<Re@@d>>, Green, Blue
         |
         |""".stripMargin,
      """|case Red: Color
         |""".stripMargin.hover
    )

  @Test def `enums2` =
    check(
      """|
         |object SimpleEnum:
         |  enum <<Col@@or>>:
         |   case Red, Green, Blue
         |
         |""".stripMargin,
      """|enum Color: SimpleEnum
         |""".stripMargin.hover
    )

  @Test def `enums-outermost` =
    check(
      """|enum Color:
         |  case Red
         |  case <<Bl@@ue>>
         |  case Cyan
         |""".stripMargin,
      """|case Blue: Color
         |""".stripMargin.hover
    )

  @Test def `enums3` =
    check(
      """|
         |object SimpleEnum:
         |  enum Color:
         |    case Red, Green, Blue
         |  val color = <<Col@@or>>.Red
         |
         |""".stripMargin,
      """|enum Color: SimpleEnum
         |""".stripMargin.hover
    )

  @Test def `enum-params` =
    check(
      """|
         |object SimpleEnum:
         |  enum Color:
         |    case <<Gr@@een>> extends Color(2)
         |    case Red extends Color(1)
         |    case Blue extends Color(3)
         |
         |
         |""".stripMargin,
      """|case Green: Color
         |""".stripMargin.hover
    )

  @Test def `extension-methods` =
    check(
      """|
         |object Foo:
         |    extension (s: String)
         |        def double = s + s
         |        def double2 = s + s
         |    end extension
         |    "".<<doub@@le2>>
         |end Foo
         |""".stripMargin,
      "extension (s: String) def double2: String".hover
    )

  /* Currently there is no way to differentiate between
   * trailing using params in extension parameter and the
   * starting using params for the actual method.
   * As user can actually supply params to them by hand when
   * invoking the extension method, we always show them next to the
   * method itself.
   * https://github.com/scala/scala3/issues/13123
   */
  @Test def `extension-methods-complex` =
    check(
      """|class A
         |class B
         |class C
         |object Foo:
         |    extension [T](using A)(s: T)(using B)
         |        def double[G <: Int](using C)(times: G) = (s.toString + s.toString) * times
         |    end extension
         |    given A with {}
         |    given B with {}
         |    given C with {}
         |    "".<<doub@@le(1)>>
         |end Foo
         |""".stripMargin,
      "extension [T](using A)(s: T) def double(using B)[G <: Int](using C)(times: G): String".hover
    )

  @Test def `extension-methods-complex-binary` =
    check(
      """|class A
         |class B
         |class C
         |
         |object Foo:
         |    extension [T](using A)(main: T)(using B)
         |      def %:[R](res: R)(using C): R = ???
         |    given A with {}
         |    given B with {}
         |    given C with {}
         |    val c = C()
         |    "" <<%@@:>> 11
         |end Foo
         |""".stripMargin,
      """|Int
         |extension [T](using A)(main: T) def %:[R](res: R)(using B)(using C): R""".stripMargin.hover
    )

  @Test def `using` =
    check(
      """
        |object a {
        |  def apply[T](a: T)(using Int): T = ???
        |  implicit val ev = 1
        |  <<ap@@ply("test")>>
        |}
        |""".stripMargin,
      """|String
         |def apply[T](a: T)(using Int): T
         |""".stripMargin.hover
    )

  @Test def `toplevel-left` =
    check(
      """|def foo = <<L@@eft>>("")
         |""".stripMargin,
      """|Left[String, Nothing]
         |def apply[A, B](value: A): Left[A, B]
         |""".stripMargin.hover
    )

  @Test def `selectable` =
    check(
      """|trait Sel extends Selectable:
         |  def selectDynamic(name: String): Any = ???
         |  def applyDynamic(name: String)(args: Any*): Any = ???
         |val sel = (new Sel {}).asInstanceOf[Sel { def foo2: Int}]
         |val foo2 = sel.fo@@o2
         |""".stripMargin,
      """|def foo2: Int
         |""".stripMargin.hover
    )

  @Test def `selectable2` =
    check(
      """|trait Sel extends Selectable:
         |  def selectDynamic(name: String): Any = ???
         |  def applyDynamic(name: String)(args: Any*): Any = ???
         |val sel = (new Sel {}).asInstanceOf[Sel { def bar2(x: Int): Int }]
         |val bar2 = sel.ba@@r2(3)
         |""".stripMargin,
      """|def bar2(x: Int): Int
         |""".stripMargin.hover
    )

  @Test def `selectable-full` =
    check(
      """|trait Sel extends Selectable:
         |  def foo1: Int = ???
         |  def bar1(x: Int): Int = ???
         |  def selectDynamic(name: String): Any = ???
         |  def applyDynamic(name: String)(args: Any*): Any = ???
         |val sel = (new Sel {}).asInstanceOf[Sel { def foo2: Int; def bar2(x: Int): Int }]
         |val bar2 = sel.fo@@o2
         |""".stripMargin,
      """|def foo2: Int
         |""".stripMargin.hover
    )

  @Test def `structural-types` =
    check(
      """|
         |import reflect.Selectable.reflectiveSelectable
         |
         |object StructuralTypes:
         |   type User = {
         |   def name: String
         |   def age: Int
         |   }
         |
         |   val user = null.asInstanceOf[User]
         |   user.name
         |   user.ag@@e
         |
         |   val V: Object {
         |   def scalameta: String
         |   } = new:
         |   def scalameta = "4.0"
         |   V.scalameta
         |end StructuralTypes
         |""".stripMargin,
      """|def age: Int
         |""".stripMargin.hover
    )

  @Test def `structural-types1` =
    check(
      """|
         |import reflect.Selectable.reflectiveSelectable
         |
         |object StructuralTypes:
         |   type User = {
         |     def name: String
         |     def age: Int
         |   }
         |
         |   val user = null.asInstanceOf[User]
         |   user.name
         |   user.age
         |
         |   val V: Object {
         |     def scalameta: String
         |   } = new:
         |     def scalameta = "4.0"
         |   V.sca@@lameta
         |end StructuralTypes
         |""".stripMargin,
      """|def scalameta: String
         |""".stripMargin.hover
    )

  @Test def `macro` =
    check(
      """|
         |import scala.quoted.*
         |
         |def myMacroImpl(using Quotes) =
         |  import quotes.reflect.Ident
         |  def foo = ??? match
         |    case x: I@@dent => x
         |
         |  def bar: Ident = foo
         |
         |  ???
         |
         |""".stripMargin,
      """|type Ident: Ident
         |""".stripMargin.hover
    )

  @Test def `macro2` =
    check(
      """|
         |
         |import scala.quoted.*
         |
         |def myMacroImpl(using Quotes) =
         |  import quotes.reflect.Ident
         |  def foo = ??? match
         |    case x: Ident => x
         |
         |  def bar: Ide@@nt = foo
         |
         |  ???
         |
         |""".stripMargin,
      """|type Ident: Ident
         |""".stripMargin.hover
    )

  @Test def `nested-selectable` =
    check(
      """|trait Sel extends Selectable:
         |  def selectDynamic(name: String): Any = ???
         |val sel = (new Sel {}).asInstanceOf[Sel { val foo: Sel { def bar: Int } }]
         |val bar = sel.foo.ba@@r
         |""".stripMargin,
      """|def bar: Int
         |""".stripMargin.hover
    )

  @Test def `nested-selectable2` =
    check(
      """|class SimpleSelectable(key : String, value: Any) extends Selectable:
         |  def selectDynamic(name: String): Any =
         |    if(name == key) value else ???
         |
         |type Node[T] = SimpleSelectable { val child: T }
         |
         |val leaf = SimpleSelectable("child", ()).asInstanceOf[Node[Unit]]
         |val node = SimpleSelectable("child", leaf).asInstanceOf[Node[Node[Unit]]]
         |
         |val k = node.child.ch@@ild
         |""".stripMargin,
      """|val child: Unit
         |""".stripMargin.hover
    )

  @Test def `very-nested-selectable` =
    check(
      """|trait Sel extends Selectable:
         |  def selectDynamic(name: String): Any = ???
         |val sel = (new Sel {}).asInstanceOf[Sel { val foo: Sel { val bar: Sel { val ddd: Int } } }]
         |val bar = sel.foo.bar.dd@@d
         |""".stripMargin,
      """|val ddd: Int
         |""".stripMargin.hover
    )

  @Test def `infix-extension` =
    check(
      """|class MyIntOut(val value: Int)
         |object MyIntOut:
         |  extension (i: MyIntOut) def uneven = i.value % 2 == 1
         |
         |object Test:
         |  val a = MyIntOut(1).un@@even
         |""".stripMargin,
      """|extension (i: MyIntOut) def uneven: Boolean
         |""".stripMargin.hover
    )

  @Test def `recursive-enum-without-type` =
    check(
      """class Wrapper(n: Int):
        |  extension (x: Int)
        |    def + (y: Int) = new Wrap@@per(x) + y
        |""".stripMargin,
      """```scala
        |def this(n: Int): Wrapper
        |```
        |""".stripMargin
    )

  @Test def `recursive-enum-without-type-1` =
    check(
      """class Wrapper(n: Int):
        |  def add(x: Int): Wrapper = ???
        |  extension (x: Int)
        |    def + (y: Int) = Wrap@@per(x).add(5)
        |""".stripMargin,
      """```scala
        |def this(n: Int): Wrapper
        |```
        |""".stripMargin
    )
