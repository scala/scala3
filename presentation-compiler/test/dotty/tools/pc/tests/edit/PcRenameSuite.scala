package dotty.tools.pc.tests.edit

import dotty.tools.pc.base.BasePcRenameSuite

import org.junit.Test

class PcRenameSuite extends BasePcRenameSuite:

  @Test def `basic` =
    check(
      """|val <<a>> = 123
         |<<@@a>> + 1
         |""".stripMargin
    )

  @Test def `apply` =
    check(
      """|object A{
         |  List(1, 2, 3).map{ _ =>
         |    val <<a>> = 123
         |    <<@@a>> + 1
         |  }
         |}
         |""".stripMargin,
      wrap = false
    )

  @Test def `generics` =
    check(
      """|trait S1[X] { def <<torename>>(p: X): String = "" }
         |trait T1[Z] extends S1[Z] { override def <<torename>>(p: Z): String = super.<<torename>>(p) }
         |trait T2[X] extends T1[X] { override def <<torename>>(p: X): String = super.<<torename>>(p) }
         |trait T3[I, J] extends T2[I] { override def <<torename>>(p: I): String = super.<<torename>>(p) }
         |trait T4[I, J] extends T3[J, I] { override def <<torename>>(p: J): String = super.<<torename>>(p) }
         |trait T5[U] extends T4[U, U] { override def <<tore@@name>>(p: U): String = super.<<torename>>(p) }
         |""".stripMargin
    )

  @Test def `match-ret-type` =
    check(
      """|trait P
         |trait PP extends P
         |trait A { def <<torename>>(a: String): P = ??? }
         |trait B extends A { override def <<tore@@name>>(a: String): PP = ??? }
         |""".stripMargin
    )

  @Test def `self-type` =
    check(
      """|trait <<A@@BC>>
         |trait Alphabet{
         |  this: <<ABC>> =>
         |}
         |object A{
         |  val a = new Alphabet with <<ABC>>
         |}
         |""".stripMargin,
      newName = "Animal"
    )

  @Test def `method-inheritance` =
    check(
      """|trait Hello{
         |  def <<method>>(abc : String) : Boolean
         |}
         |
         |class GoodMorning extends Hello {
         |  def <<met@@hod>>(abc : String) = true
         |}
         |""".stripMargin
    )

  @Test def `long-inheritance` =
    check(
      """|trait A[T, S] {
         |  def <<method>>(abc : T) : S
         |}
         |
         |abstract class B[T] extends A[T, Boolean] {
         |  def <<method>>(abc : T) : Boolean
         |}
         |
         |abstract class C extends B[String] {
         |  def <<meth@@od>>(abc : String) : Boolean = false
         |}
         |""".stripMargin
    )

  @Test def `multiple-inheritance` =
    check(
      """|trait A {
         |  def <<method>>(abc : String) : Boolean
         |}
         |
         |trait B {
         |  def <<method>>(abc : String) : Boolean = true
         |}
         |
         |abstract class C extends B with A {
         |  override def <<meth@@od>>(abc : String) : Boolean = false
         |}
         |""".stripMargin
    )

  @Test def `colon-bad` =
    check(
      """|class User{
         |  def <<:@@:>>(name : String) = name
         |}
         |object Main{
         |  val user = new User()
         |  "" <<::>> user
         |}
         |""".stripMargin
    )

  @Test def `colon-good` =
    check(
      """|class User{
         |  def <<:@@:>>(name : String) = name
         |}
         |object Main{
         |  val user = new User()
         |  "" <<::>> user
         |}
         |""".stripMargin,
      newName = "+++:"
    )

  @Test def `inheritance` =
    check(
      """|abstract class <<An@@imal>>
         |class Dog extends <<Animal>>
         |class Cat extends <<Animal>>
         |""".stripMargin,
      newName = "Tree"
    )

  @Test def `companion` =
    check(
      """|class <<Foo>>{}
         |object <<Fo@@o>> {}
    """.stripMargin,
      newName = "Tree"
    )

  @Test def `companion2` =
    check(
      """|class <<Fo@@o>>{}
         |object <<Foo>>
    """.stripMargin,
      newName = "Tree"
    )

  @Test def `macro` =
    check(
      """|iort io.circe.generic.JsonCodec
         |trait LivingBeing
         |@JsonCodec sealed trait <<An@@imal>> extends LivingBeing
         |object <<Animal>> {
         |  case object Dog extends <<Animal>>
         |  case object Cat extends <<Animal>>
         |}
         |""".stripMargin
    )

  @Test def `anon` =
    check(
      """|trait Methodable[T] {
         |  def <<method>>(asf: T): Int
         |}
         |
         |trait Alphabet extends Methodable[String] {
         |  def <<method>>(adf: String) = 123
         |}
         |
         |val a = new Alphabet {
         |  override def <<me@@thod>>(adf: String): Int = 321
         |}
         |""".stripMargin
    )

  @Test def `implicit-param` =
    check(
      """|implicit val <<some@@Name>>: Int = 1
         |def m[A](implicit a: A): A = a
         |m[Int]
         |""".stripMargin
    )

  @Test def `backtick-new-name` =
    check(
      """|object A{
         |  val <<toRename>> = 123
         |}
         |object B{
         |  val toRename = A.<<toR@@ename>>
         |}
         |""".stripMargin,
      newName = "`other-rename`"
    )

  @Test def `backtick-old-and-new-name` =
    check(
      """|object A{
         |  val <<`to-Rename`>> = 123
         |}
         |object B{
         |  val toRename = A.<<`to-R@@ename`>>
         |}
         |""".stripMargin,
      newName = "`other-rename`"
    )

  @Test def `backtick` =
    check(
      """|val <<greet@@ing>> = "Hello"
         |"" match {
         |  case `<<greeting>>` =>
         |}
         |""".stripMargin
    )

  @Test def `double-backtick` =
    check(
      """|val <<greet@@ing>> = "Hello"
         |"" match {
         |  case <<`greeting`>> =>
         |}
         |""".stripMargin,
      newName = "`greeting-!`"
    )

  @Test def `backtick2` =
    check(
      """|val <<greeting>> = "Hello"
         |"" match {
         |  case `<<gre@@eting>>` =>
         |}
         |""".stripMargin
    )

  @Test def `backtick3` =
    check(
      """|val greeting = "Hello"
         |"" match {
         |  case `gre@@eting` =>
         |}
         |""".stripMargin,
      newName = "`greeting`"
    )

  @Test def `params1` =
    check(
      """|case class Name(<<value>>: String)
         |val name1 = Name(<<value>> = "42")
         | .copy(<<value>> = "43")
         | .copy(<<va@@lue>> = "43")
         | .<<value>>
         |val name2 = Name(<<value>> = "44")
         |""".stripMargin
    )

  @Test def `params2` =
    check(
      """|case class Name(<<value>>: String)
         |val name1 = Name(<<val@@ue>> = "42")
         | .copy(<<value>> = "43")
         | .copy(<<value>> = "43")
         | .<<value>>
         |val name2 = Name(<<value>> = "44")
         |""".stripMargin
    )

  @Test def `constructor` =
    check(
      """|case class Name(<<va@@lue>>: String)
         |val name2 = new Name(<<value>> = "44")
         |""".stripMargin
    )

  @Test def `type-params1` =
    check(
      """|trait <<ABC>>
         |class CBD[T <: <<ABC>>]
         |val a = classOf[<<AB@@C>>]
         |val b = new CBD[<<ABC>>]
         |""".stripMargin,
      newName = "Animal"
    )

  @Test def `type-params2` =
    check(
      """|trait <<A@@BC>>
         |class CBD[T <: <<ABC>>]
         |val a = classOf[<<ABC>>]
         |val b = new CBD[<<ABC>>]
         |""".stripMargin,
      newName = "Animal"
    )

  @Test def `implicit-parameters` =
    check(
      """|trait A {
         | implicit def <<foo>>: Double
         |}
         |object A extends A {
         |  implicit def <<fo@@o>>: Double = 0.1
         |  def bar(implicit x: Double): Double = x
         |  val x = bar
         |}
         |""".stripMargin,
      newName = "foo2"
    )

  @Test def `hierarchy-trait` =
    check(
      """|sealed trait <<Sy@@mbol>>
         |case class Method(name: String) extends <<Symbol>>
         |case class Variable(value: String) extends <<Symbol>>
         |
         |val symbol2: <<Symbol>> = Method("method")
         |val symbol3: <<Symbol>> = Variable("value")
         |""".stripMargin,
      newName = "NewSymbol"
    )

  @Test def `hierarchy-class` =
    check(
      """|sealed abstract class <<Sy@@mbol>>
         |case class Method(name: String) extends <<Symbol>>
         |case class Variable(value: String) extends <<Symbol>>
         |
         |val symbol2: <<Symbol>> = Method("method")
         |val symbol3: <<Symbol>> = Variable("value")
         |""".stripMargin,
      newName = "NewSymbol"
    )

  @Test def `variable` =
    check(
      """|  var <<v@@5>> = false
         |
         |  def f5: Boolean = {
         |    <<v5>> = true
         |    <<v5>> == true
         |  }
         |""".stripMargin
    )

  @Test def `worksheet-method` =
    check(
      """|trait S1[X] { def <<torename>>(p: X): String = "" }
         |trait T1[Z] extends S1[Z] { override def <<tore@@name>>(p: Z): String = super.<<torename>>(p) }
         |""".stripMargin,
      filename = "A.worksheet.sc",
      wrap = false
    )

  @Test def `worksheet-classes` =
    check(
      """|sealed abstract class <<Sy@@mbol>>
         |case class Method(name: String) extends <<Symbol>>
         |case class Variable(value: String) extends <<Symbol>>
         |""".stripMargin,
      newName = "Tree",
      filename = "A.worksheet.sc",
      wrap = false
    )

  @Test def `not-compiling` =
    check(
      """|package a
         |object Main {
         |  def method() = {
         |    List(1) + 2
         |    val <<abc>>: Option[Int] = ???
         |    <<ab@@c>>.map(_ + 1)
         |  }
         |}
         |""".stripMargin
    )

  @Test def `extension-param` =
    check(
      """|extension (<<sb@@d>>: String)
         |  def double = <<sbd>> + <<sbd>>
         |  def double2 = <<sbd>> + <<sbd>>
         |end extension
         |""".stripMargin,
      newName = "greeting"
    )

  @Test def `extension-params-ref` =
    check(
      """|extension (<<sbd>>: String)
         |  def double = <<sb@@d>> + <<sbd>>
         |  def double2 = <<sbd>> + <<sbd>>
         |end extension
         |""".stripMargin,
      newName = "greeting"
    )

  @Test def `extension-type-param` =
    check(
      """|extension [T](<<x@@s>>: List[T])
         |  def double = <<xs>> ++ <<xs>>
         |  def double2 = <<xs>> ++ <<xs>>
         |end extension
         |""".stripMargin,
      newName = "ABC"
    )

  @Test def `extension-type-param-ref` =
    check(
      """|extension [T](<<xs>>: List[T])
         |  def double = <<xs>> ++ <<xs>>
         |  def double2 = <<xs>> ++ <<x@@s>>
         |end extension
         |""".stripMargin,
      newName = "ABC"
    )

  @Test def `class-of` =
    check(
      """|object Test {
         |  classOf[String]
         |  def test() = {
         |    val <<tes@@tVal>> = "test"
         |  }
         |}
         |""".stripMargin,
      newName = "testing"
    )

  @Test def `trailling-comma` =
    check(
      """|object A{
         |  val <<`to-Rename`>> = 123
         |}
         |object B{
         |  val toRename =
         |    List(
         |      A.<<`to-R@@ename`>>,
         |      A.<<`to-Rename`>>,
         |    )
         |}
         |""".stripMargin,
      newName = "`other-rename`"
    )

  @Test def `for-comp-bind` =
    check(
      """
        |case class Bar(fooBar: Int, goo: Int)
        |val abc = for {
        |  foo <- List(1)
        |  _ = Option(1)
        |  Bar(<<fooBar>>, goo) <- List(Bar(foo, 123))
        |  baz = <<fooBar>> + goo
        |} yield {
        |  val x = foo + <<foo@@Bar>> + baz
        |  x
        |}""".stripMargin
    )
  @Test def `for-comprehension` =
    check(
      """|val a = for {
         |  <<ab@@c>> <- List("a", "b", "c")
         |  _ = println("print!")
         |} yield <<a@@bc>>
         |""".stripMargin
    )

  @Test def `map-method` =
    check(
      """|case class Bar(x: List[Int]) {
         |  def <<ma@@p>>(f: Int => Int): Bar = Bar(x.map(f))
         |}
         |
         |val f =
         |  for {
         |    b <- Bar(List(1,2,3))
         |  } yield b
         |""".stripMargin
    )

  @Test def `end-marker` =
    check(
      """|def <<he@@llo>>(a: Int) =
        |  ???
        |end <<hello>>
        |""".stripMargin
    )

  @Test def `end-marker-with-comment` =
    check(
      """|def <<he@@llo>>(a: Int) =
        |  ???
        |end /* a comment */ <<hello>> /* a comment */
        |""".stripMargin
    )

  @Test def `end-marker-wrong` =
    check(
      """|def <<f@@oo>> =
        |  def bar =
        |    ???
        |  end bar""".stripMargin
    )

  @Test def `apply-rename` =
    check(
      """|object B {
        |  def locally = {
        |    object A{ def app@@ly(a: Int) = ??? }
        |    A(123)
        |    A.apply(123)
        |  }
        |}
        |""".stripMargin,
      wrap = false
    )

  @Test def `constructor-rename` =
    check(
      """|object B {
        |  def locally = {
        |    class A(a : String){ def th@@is(a: Int) = this(a.toString) }
        |    A(123)
        |    A.apply(123)
        |  }
        |}
        |""".stripMargin,
      wrap = false
    )

  @Test def `local-object-with-end-rename` =
    check(
      """|def bar =
        |  object <<fo@@o>>:
        |    def aaa = ???
        |  end <<foo>>
        |  1
        |""".stripMargin,
      wrap = false
    )

  // Named tuple fields have no symbol; they are declared by the type literal
  @Test def `named-tuple-field` =
    check(
      """|val x: (<<name>>: String, age: Int) = (<<name>> = "a", age = 1)
         |val other: (<<name>>: String, age: Int) = ???
         |val car: (name: String, hp: Int) = ???
         |val y = x.<<na@@me>>
         |val z = x.age
         |val otherName = other.<<name>>
         |val carName = car.name
         |""".stripMargin
    )

  @Test def `named-tuple-field-nested` =
    check(
      """|val x: (inner: (<<name>>: String, age: Int)) = ???
         |val y = x.inner.<<na@@me>>
         |val z = x.inner.age
         |""".stripMargin
    )

  // The type may be used from other files, so only local declarations are renamed
  @Test def `named-tuple-field-member` =
    check(
      """|object Main {
         |  val x: (name: String, age: Int) = ???
         |  def m() = x.na@@me
         |}
         |""".stripMargin,
      wrap = false
    )

  @Test def `named-tuple-field-param` =
    check(
      """|object Main {
         |  def m(p: (name: String, age: Int)) = p.na@@me
         |}
         |""".stripMargin,
      wrap = false
    )

  // Pattern labels are found in the untyped tree, the typer drops them
  @Test def `named-tuple-field-pattern` =
    check(
      """|val x: (<<name>>: String, age: Int) = (<<name>> = "a", age = 1)
         |x match
         |  case (<<name>> = n, age = a) => println(n)
         |  case (age = a, <<name>> = n) => println(n)
         |val y = x.<<na@@me>>
         |""".stripMargin
    )

  @Test def `named-tuple-field-pattern-nested` =
    check(
      """|val x: (outer: (<<name>>: String)) = ???
         |x match
         |  case (outer = (<<na@@me>> = n)) => println(n)
         |  case (outer = o) => println(o.<<name>>)
         |""".stripMargin
    )

  @Test def `named-tuple-field-pattern-for` =
    check(
      """|val xs: List[(<<name>>: String)] = Nil
         |for (<<name>> = n) <- xs do println(n)
         |val (<<na@@me>> = m) = xs.head
         |""".stripMargin
    )

  @Test def `named-tuple-field-subtype-constructor` =
    check(
      """|val x: (<<o>>: Option[Int], xs: List[Int]) = (<<o>> = Some(1), xs = Nil)
         |val y = x.<<o@@>>
         |""".stripMargin
    )

  @Test def `named-tuple-field-generic-constructor` =
    check(
      """|def mk[T](t: T) = (<<v>> = t)
         |val y = mk(1).<<v@@>>
         |val z: (<<v>>: String) = mk("a")
         |""".stripMargin
    )

  @Test def `named-tuple-field-backticked` =
    check(
      """|val x: (<<`my-name`>>: String) = (<<`my-name`>> = "a")
         |val y = x.<<`my-n@@ame`>>
         |""".stripMargin
    )

  @Test def `named-tuple-field-to-backticked` =
    check(
      """|val x: (<<name>>: String) = (<<name>> = "a")
         |val y = x.<<na@@me>>
         |""".stripMargin,
      newName = "`my-name`"
    )

  @Test def `named-tuple-field-explicit-literal` =
    check(
      """|val x: NamedTuple.NamedTuple[("<<name>>", "age"), (String, Int)] = ???
         |val y = x.<<na@@me>>
         |""".stripMargin
    )

  @Test def `named-tuple-field-same-name-other-symbols` =
    check(
      """|val name = "zzz"
         |val x: (<<name>>: String) = (<<name>> = name)
         |val y = x.<<na@@me>>
         |def f(name: Int) = name
         |""".stripMargin
    )

  // the type also comes from `NamedTuple.From[P]`, which cannot be renamed

  @Test def `named-tuple-field-local-alias` =
    check(
      """|type P = (<<name>>: String)
         |val p: P = (<<name>> = "a")
         |val n = p.<<na@@me>>
         |""".stripMargin
    )

  @Test def `named-tuple-field-constructor-in-body` =
    check(
      """|val x: (<<name>>: String) = ???
         |println((<<name>> = "b").<<na@@me>>)
         |""".stripMargin
    )

  // param of a local method is local

  @Test def `named-tuple-field-local-method-param` =
    check(
      """|def inner(p: (<<name>>: String)) = p.<<na@@me>>
         |""".stripMargin
    )

  @Test def `named-tuple-field-lambda-param` =
    check(
      """|val xs: List[(<<name>>: String)] = Nil
         |xs.map(x => x.<<na@@me>>)
         |""".stripMargin
    )

  @Test def `named-tuple-field-member-statement` =
    check(
      """|object Main {
         |  println((<<name>> = "a").<<na@@me>>)
         |}
         |""".stripMargin,
      wrap = false
    )

  @Test def `named-tuple-field-pattern-val` =
    check(
      """|val (a, b) = ((<<name>> = "a"), 1)
         |val y = a.<<na@@me>>
         |""".stripMargin
    )

  @Test def `named-tuple-field-worksheet` =
    check(
      """|object Main {
         |  val x: (<<name>>: String) = (<<name>> = "a")
         |  def m() = x.<<na@@me>>
         |}
         |""".stripMargin,
      filename = "A.worksheet.sc",
      wrap = false
    )

  // The type escapes the file through a signature visible outside, so it cannot be renamed
  @Test def `named-tuple-field-inferred-member-type` =
    check(
      """|object Main {
         |  def m = {
         |    val x: (name: String) = (name = "a")
         |    x
         |  }
         |  def n = m.na@@me
         |}
         |""".stripMargin,
      wrap = false
    )

  // a constructor directly in a member method body is local

  @Test def `named-tuple-field-inferred-member-val` =
    check(
      """|object Main {
         |  val v = { val x: (name: String) = (name = "a"); x }
         |  def n = v.na@@me
         |}
         |""".stripMargin,
      wrap = false
    )

  @Test def `named-tuple-field-inferred-member-list` =
    check(
      """|object Main {
         |  val xs = List((name = "a"))
         |  def m() = xs.head.na@@me
         |}
         |""".stripMargin,
      wrap = false
    )

  @Test def `named-tuple-field-member-result-type` =
    check(
      """|object Main {
         |  def m: (name: String) = (name = "a")
         |  def n = m.na@@me
         |}
         |""".stripMargin,
      wrap = false
    )

  @Test def `named-tuple-field-member-lambda-param` =
    check(
      """|object Main {
         |  val f = (p: (name: String)) => p.na@@me
         |}
         |""".stripMargin,
      wrap = false
    )

  @Test def `named-tuple-field-type-param-bound` =
    check(
      """|object Main {
         |  class Box[T <: (name: String)](t: T) { def n = t.na@@me }
         |}
         |""".stripMargin,
      wrap = false
    )

  @Test def `named-tuple-field-member-class-parent` =
    check(
      """|object Main {
         |  class Box[T]
         |  class C extends Box[(name: String)]
         |  def m() = { val x: (name: String) = ???; x.na@@me }
         |}
         |""".stripMargin,
      wrap = false
    )

  @Test def `named-tuple-field-member-alias` =
    check(
      """|object Main {
         |  type P = (name: String)
         |  val x: P = ???
         |  def m() = { val y: (name: String) = x; y.na@@me }
         |}
         |""".stripMargin,
      wrap = false
    )

  // The type also comes from elsewhere, so it cannot be renamed
  @Test def `named-tuple-field-from-case-class` =
    check(
      """|case class P(name: String, age: Int)
         |val p: NamedTuple.From[P] = ???
         |val x: (name: String, age: Int) = ???
         |val y = x.na@@me
         |val z = p.name
         |""".stripMargin
    )

  @Test def `named-tuple-field-other-object` =
    check(
      """|object Main {
         |  def m() = {
         |    val x: (name: String, age: Int) = ???
         |    val y = x.na@@me
         |    val z = Other.p.name
         |  }
         |}
         |object Other { def p: (name: String, age: Int) = ??? }
         |""".stripMargin,
      wrap = false
    )
