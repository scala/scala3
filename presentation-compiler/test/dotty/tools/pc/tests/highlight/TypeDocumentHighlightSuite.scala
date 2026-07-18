package dotty.tools.pc.tests.highlight

import dotty.tools.pc.base.BaseDocumentHighlightSuite

import org.junit.{Ignore, Test}

class TypeDocumentHighlightSuite extends BaseDocumentHighlightSuite:

  @Test def `type1` =
    check(
      """
        |object Test {
        |  type <<NotI@@nt>> = Int
        |  val set = Set.empty[<<NotInt>>]
        |}""".stripMargin
    )

  @Test def `type2` =
    check(
      """
        |object Test {
        |  type <<NotInt>> = Int
        |  val set = Set.empty[<<Not@@Int>>]
        |}""".stripMargin
    )

  @Test def `type3` =
    check(
      """
        |object Test {
        |  type NotInt = <<In@@t>>
        |  val set = Set.empty[<<Int>>]
        |}""".stripMargin
    )

  @Test def `type4` =
    check(
      """
        |object Test {
        |  type NotInt = <<Int>>
        |  val set = Set.empty[<<I@@nt>>]
        |}""".stripMargin
    )

  @Test def `type-in-def2` =
    check(
      """
        |object Test {
        |  var bspSession: Option[<<Stri@@ng>>] =
        |    Option.empty[<<String>>]
        |}""".stripMargin
    )

  @Test def `type-in-def3` =
    check(
      """
        |object Test {
        |  var bspSession: <<Op@@tion>>[String] =
        |    <<Option>>.empty[String]
        |}""".stripMargin
    )

  @Test def `type-in-def4` =
    check(
      """
        |object Test {
        |  var bspSession: <<Option>>[String] =
        |    <<Opt@@ion>>.empty[String]
        |}""".stripMargin
    )

  @Test def `type-bounds1` =
    check(
      """
        |object Test {
        |  type A = List[_ <: <<It@@erable>>[Int]]
        |  val a : <<Iterable>>[Int] = ???
        |}""".stripMargin
    )

  @Test def `type-bounds2` =
    check(
      """
        |object Test {
        |  type A = List[_ <: <<Iterable>>[Int]]
        |  val a : <<Ite@@rable>>[Int] = ???
        |}""".stripMargin
    )

  @Test def `type-bounds3` =
    check(
      """
        |object Test {
        |  type A = List[_ <: scala.<<Enumerati@@on>>]
        |  val a : scala.<<Enumeration>> = ???
        |}""".stripMargin
    )

  @Test def `type-bounds4` =
    check(
      """
        |object Test {
        |  type A = List[_ <: scala.<<Enumeration>>]
        |  val a : scala.<<Enumer@@ation>> = ???
        |}""".stripMargin
    )

  @Test def `type-bounds5` =
    check(
      """
        |object Test {
        |  type A = List[_ <: Iterable[<<In@@t>>]]
        |  val a : Iterable[<<Int>>] = ???
        |}""".stripMargin
    )

  @Test def `type-bounds6` =
    check(
      """
        |object Test {
        |  type A = List[_ <: Iterable[<<Int>>]]
        |  val a : Iterable[<<In@@t>>] = ???
        |}""".stripMargin
    )

  @Test def `annot1` =
    check(
      """
        |object Test {
        |  @<<depr@@ecated>>(since = "1.23")
        |  val hello1 = 123
        |
        |  @<<deprecated>>(since = "1.23")
        |  val hello2 = 123
        |}""".stripMargin
    )

  @Test def `annot2` =
    check(
      """
        |object Test {
        |  @deprecated(<<since>> = "1.23")
        |  val hello1 = 123
        |
        |  @deprecated(<<si@@nce>> = "1.23")
        |  val hello2 = 123
        |}""".stripMargin
    )

  @Ignore @Test def `projection1` =
    check(
      """|
         |class A {
         |    type <<B@@B>> = Int
         |  }
         |  object Test {
         |    val b1: A#<<BB>> = 12
         |}""".stripMargin
    )

  @Ignore @Test def `projection2` =
    check(
      """|
         |class A {
         |    type <<BB>> = Int
         |  }
         |  object Test {
         |    val b1: A#<<B@@B>> = 12
         |}""".stripMargin
    )

  @Test def `trait-param1` =
    check(
      """|trait Zg[<<T@@T>>]{
         |  def doZ: List[<<TT>>]
         |}
         |""".stripMargin
    )

  @Test def `trait-param2` =
    check(
      """|trait Zg[<<TT>>]{
         |  def doZ: List[<<T@@T>>]
         |}
         |""".stripMargin
    )

  @Test def `symbolic-type` =
    check(
      """|object A {
         |  type <<!!>>[+T, -U] = Int
         |  def m(x: Int <<!@@!>> String) = ???
         |}
         |""".stripMargin
    )
