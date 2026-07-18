package dotty.tools.pc.tests.tokens

import java.nio.file.Path

import dotty.tools.pc.base.BaseSemanticTokensSuite

import org.junit.Test

class SemanticTokensSuite extends BaseSemanticTokensSuite:

  @Test def `class, object, var, val(readonly), method, type, parameter, String(single-line)` =
    check(
      s"""|package <<example>>/*namespace*/
          |
          |class <<Test>>/*class*/{
          |
          |  var <<wkStr>>/*variable,definition*/ = "Dog-"
          |  val <<nameStr>>/*variable,definition,readonly*/ = "Jack"
          |
          |  def <<Main>>/*method,definition*/={
          |
          |    val <<preStr>>/*variable,definition,readonly*/= "I am "
          |    var <<postStr>>/*variable,definition*/= "in a house. "
          |    <<wkStr>>/*variable*/=<<nameStr>>/*variable,readonly*/ <<+>>/*method*/ "Cat-"
          |
          |    <<testC>>/*class*/.<<bc>>/*method*/(<<preStr>>/*variable,readonly*/
          |      <<+>>/*method*/ <<wkStr>>/*variable*/
          |      <<+>>/*method*/ <<preStr>>/*variable,readonly*/)
          |  }
          |}
          |
          |object <<testC>>/*class*/{
          |
          |  def <<bc>>/*method,definition*/(<<msg>>/*parameter,declaration,readonly*/:<<String>>/*type*/)={
          |    <<println>>/*method*/(<<msg>>/*parameter,readonly*/)
          |  }
          |}
          |""".stripMargin
    )

  @Test def `metals-6823` =
    check(
      s"""|package <<example>>/*namespace*/
            |
            | @<<main>>/*class*/ def <<main1>>/*method,definition*/(): <<Unit>>/*class,abstract*/ =
            |     val <<array>>/*variable,definition,readonly*/ = <<Array>>/*class*/(1, 2, 3)
            |     <<println>>/*method*/(<<array>>/*variable,readonly*/)
            |
            |@<<main>>/*class*/ def <<main2>>/*method,definition*/(): <<Unit>>/*class,abstract*/ =
            |   val <<list>>/*variable,definition,readonly*/ = <<List>>/*class*/(1, 2, 3)
            |   <<println>>/*method*/(<<list>>/*variable,readonly*/)
            |
            |@<<main>>/*class*/ def <<main3>>/*method,definition*/(): <<Unit>>/*class,abstract*/ =
            |   val <<list>>/*variable,definition,readonly*/ = <<List>>/*class*/(1, 2, 3)
            |   <<println>>/*method*/(<<list>>/*variable,readonly*/)
            |""".stripMargin
    )

  @Test def `Comment(Single-Line, Multi-Line)` =
    check(
      s"""|package <<example>>/*namespace*/
          |
          |object <<Main>>/*class*/{
          |
          |   /**
          |   * Test of Comment Block
          |   */  val <<x>>/*variable,definition,readonly*/ = 1
          |
          |  def <<add>>/*method,definition*/(<<a>>/*parameter,declaration,readonly*/ : <<Int>>/*class,abstract*/) = {
          |    // Single Line Comment
          |    <<a>>/*parameter,readonly*/ <<+>>/*method*/ 1 // com = 1
          |  }
          |}
          |""".stripMargin
    )

  @Test def `number literal, Static` =
    check(
      s"""|package <<example>>/*namespace*/
          |
          |object <<ab>>/*class*/ {
          |  var  <<iVar>>/*variable,definition*/:<<Int>>/*class,abstract*/ = 1
          |  val  <<iVal>>/*variable,definition,readonly*/:<<Double>>/*class,abstract*/ = 4.94065645841246544e-324d
          |  val  <<fVal>>/*variable,definition,readonly*/:<<Float>>/*class,abstract*/ = 1.40129846432481707e-45
          |  val  <<lVal>>/*variable,definition,readonly*/:<<Long>>/*class,abstract*/ = 9223372036854775807L
          |}
          |
          |object <<sample10>>/*class*/ {
          |  def <<main>>/*method,definition*/(<<args>>/*parameter,declaration,readonly*/: <<Array>>/*class*/[<<String>>/*type*/]) ={
          |    <<println>>/*method*/(
          |      (<<ab>>/*class*/.<<iVar>>/*variable*/ <<+>>/*method*/ <<ab>>/*class*/.<<iVal>>/*variable,readonly*/).<<toString>>/*method*/
          |    )
          |  }
          |}
          |""".stripMargin
    )

  @Test def `abstract(modifier), trait, type parameter` =
    check(
      s"""|
          |package <<a>>/*namespace*/.<<b>>/*namespace*/
          |object <<Sample5>>/*class*/ {
          |
          |  type <<PP>>/*type,definition*/ = <<Int>>/*class,abstract*/
          |  def <<main>>/*method,definition*/(<<args>>/*parameter,declaration,readonly*/: <<Array>>/*class*/[<<String>>/*type*/]) ={
          |      val <<itr>>/*variable,definition,readonly*/ = new <<IntIterator>>/*class*/(5)
          |      var <<str>>/*variable,definition*/ = <<itr>>/*variable,readonly*/.<<next>>/*method*/().<<toString>>/*method*/ <<+>>/*method*/ ","
          |          <<str>>/*variable*/ += <<itr>>/*variable,readonly*/.<<next>>/*method*/().<<toString>>/*method*/
          |      <<println>>/*method*/("count:"<<+>>/*method*/<<str>>/*variable*/)
          |  }
          |
          |  trait <<Iterator>>/*interface,abstract*/[<<A>>/*typeParameter,definition,abstract*/] {
          |    def <<next>>/*method,declaration*/(): <<A>>/*typeParameter,abstract*/
          |  }
          |
          |  abstract class <<hasLogger>>/*class,abstract*/ {
          |    def <<log>>/*method,definition*/(<<str>>/*parameter,declaration,readonly*/:<<String>>/*type*/) = {<<println>>/*method*/(<<str>>/*parameter,readonly*/)}
          |  }
          |
          |  class <<IntIterator>>/*class*/(<<to>>/*variable,declaration,readonly*/: <<Int>>/*class,abstract*/)
          |  extends <<hasLogger>>/*class,abstract*/ with <<Iterator>>/*interface,abstract*/[<<Int>>/*class,abstract*/]  {
          |    private var <<current>>/*variable,definition*/ = 0
          |    override def <<next>>/*method,definition*/(): <<Int>>/*class,abstract*/ = {
          |      if (<<current>>/*variable*/ <<<>>/*method*/ <<to>>/*variable,readonly*/) {
          |        <<log>>/*method*/("main")
          |        val <<t>>/*variable,definition,readonly*/ = <<current>>/*variable*/
          |        <<current>>/*variable*/ = <<current>>/*variable*/ <<+>>/*method*/ 1
          |        <<t>>/*variable,readonly*/
          |      } else 0
          |    }
          |  }
          |}
          |
          |
          |""".stripMargin
    )

  @Test def `deprecated` =
    check(
      s"""|package <<example>>/*namespace*/
          |object <<sample9>>/*class*/ {
          |  @<<deprecated>>/*class*/("this method will be removed", "FooLib 12.0")
          |  def <<oldMethod>>/*method,definition,deprecated*/(<<x>>/*parameter,declaration,readonly*/: <<Int>>/*class,abstract*/) = <<x>>/*parameter,readonly*/
          |
          |  def <<main>>/*method,definition*/(<<args>>/*parameter,declaration,readonly*/: <<Array>>/*class*/[<<String>>/*type*/]) ={
          |    val <<str>>/*variable,definition,readonly*/ = <<oldMethod>>/*method,deprecated*/(2).<<toString>>/*method*/
          |     <<println>>/*method*/("Hello, world!"<<+>>/*method*/ <<str>>/*variable,readonly*/)
          |  }
          |}
          |""".stripMargin
    )

  @Test def `import(Out of File)` =
    check(
      s"""|package <<example>>/*namespace*/
          |
          |import <<scala>>/*namespace*/.<<collection>>/*namespace*/.<<immutable>>/*namespace*/.<<SortedSet>>/*class*/
          |
          |object <<sample3>>/*class*/ {
          |
          |  def <<sorted1>>/*method,definition*/(<<x>>/*parameter,declaration,readonly*/: <<Int>>/*class,abstract*/)
          |     = <<SortedSet>>/*class*/(<<x>>/*parameter,readonly*/)
          |}
          |
          |""".stripMargin
    )

  @Test def `anonymous-class` =
    check(
      s"""|package <<example>>/*namespace*/
          |object <<A>>/*class*/ {
          |  trait <<Methodable>>/*interface,abstract*/[<<T>>/*typeParameter,definition,abstract*/] {
          |    def <<method>>/*method,declaration*/(<<asf>>/*parameter,declaration,readonly*/: <<T>>/*typeParameter,abstract*/): <<Int>>/*class,abstract*/
          |  }
          |
          |  abstract class <<Alp>>/*class,abstract*/(<<alp>>/*variable,declaration,readonly*/: <<Int>>/*class,abstract*/) extends <<Methodable>>/*interface,abstract*/[<<String>>/*type*/] {
          |    def <<method>>/*method,definition*/(<<adf>>/*parameter,declaration,readonly*/: <<String>>/*type*/) = 123
          |  }
          |  val <<a>>/*variable,definition,readonly*/ = new <<Alp>>/*class,abstract*/(<<alp>>/*parameter,readonly*/ = 10) {
          |    override def <<method>>/*method,definition*/(<<adf>>/*parameter,declaration,readonly*/: <<String>>/*type*/): <<Int>>/*class,abstract*/ = 321
          |  }
          |}""".stripMargin
    )

  @Test def `import-rename` =
    check(
      s"""|package <<example>>/*namespace*/
          |
          |import <<util>>/*namespace*/.{<<Failure>>/*class*/ => <<NoBad>>/*class*/}
          |
          |class <<Imports>>/*class*/ {
          |  // rename reference
          |  <<NoBad>>/*class*/(null)
          |}""".stripMargin
    )

  @Test def `pattern-match` =
    check(
      s"""|package <<example>>/*namespace*/
          |
          |class <<Imports>>/*class*/ {
          |
          |  val <<a>>/*variable,definition,readonly*/ = <<Option>>/*class*/(<<Option>>/*class*/(""))
          |  <<a>>/*variable,readonly*/ match {
          |    case <<Some>>/*class*/(<<Some>>/*class*/(<<b>>/*variable,definition,readonly*/)) => <<b>>/*variable,readonly*/
          |    case <<Some>>/*class*/(<<b>>/*variable,definition,readonly*/) => <<b>>/*variable,readonly*/
          |    case <<other>>/*variable,definition,readonly*/ =>
          |  }
          |}""".stripMargin
    )

  @Test def `pattern-match-value` =
    check(
      s"""|package <<example>>/*namespace*/
          |
          |object <<A>>/*class*/ {
          |  val <<x>>/*variable,definition,readonly*/ = <<List>>/*class*/(1,2,3)
          |  val <<s>>/*variable,definition,readonly*/ = <<Some>>/*class*/(1)
          |  val <<Some>>/*class*/(<<s1>>/*variable,definition,readonly*/) = <<s>>/*variable,readonly*/
          |  val <<Some>>/*class*/(<<s2>>/*variable,definition,readonly*/) = <<s>>/*variable,readonly*/
          |}
          |""".stripMargin
    )

  @Test def `enum` =
    check(
      """|package <<example>>/*namespace*/
         |
         |enum <<FooEnum>>/*enum,abstract*/:
         |  case <<Bar>>/*enum*/, <<Baz>>/*enum*/
         |object <<FooEnum>>/*class*/
         |""".stripMargin
    )

  @Test def `enum1` =
    check(
      """|package <<example>>/*namespace*/
         |
         |enum <<FooEnum>>/*enum,abstract*/:
         |  case <<A>>/*enum*/(<<a>>/*variable,declaration,readonly*/: <<Int>>/*class,abstract*/)
         |  case <<B>>/*enum*/(<<a>>/*variable,declaration,readonly*/: <<Int>>/*class,abstract*/, <<b>>/*variable,declaration,readonly*/: <<Int>>/*class,abstract*/)
         |  case <<C>>/*enum*/(<<a>>/*variable,declaration,readonly*/: <<Int>>/*class,abstract*/, <<b>>/*variable,declaration,readonly*/: <<Int>>/*class,abstract*/, <<c>>/*variable,declaration,readonly*/: <<Int>>/*class,abstract*/)
         |
         |""".stripMargin
    )

  // Issue: Sequential parameters are not highlighted
  // https://github.com/scalameta/metals/issues/4985
  @Test def `named-arguments` =
    check(
      s"""|package <<example>>/*namespace*/
          |
          |def <<m>>/*method,definition*/(<<xs>>/*parameter,declaration,readonly*/: <<Int>>/*class,abstract*/*) = <<xs>>/*parameter,readonly*/.<<map>>/*method*/(<<_>>/*parameter,readonly*/ <<+>>/*method*/ 1)
          |val <<a>>/*variable,definition,readonly*/ = <<m>>/*method*/(xs = 1,2,3)
          |""".stripMargin
    )

  // Issue: Structural types are not highlighted
  // https://github.com/scalameta/metals/issues/4984
  @Test def `structural-types` =
    check(
      s"""|package <<example>>/*namespace*/
          |
          |import <<reflect>>/*namespace*/.<<Selectable>>/*class*/.<<reflectiveSelectable>>/*method*/
          |
          |object <<StructuralTypes>>/*class*/:
          |  type <<User>>/*type,definition*/ = {
          |    def <<name>>/*method,declaration*/: <<String>>/*type*/
          |    def <<age>>/*method,declaration*/: <<Int>>/*class,abstract*/
          |  }
          |
          |  val <<user>>/*variable,definition,readonly*/ = null.<<asInstanceOf>>/*method*/[<<User>>/*type*/]
          |  <<user>>/*variable,readonly*/.<<name>>/*method*/
          |  <<user>>/*variable,readonly*/.<<age>>/*method*/
          |
          |  val <<V>>/*variable,definition,readonly*/: <<Object>>/*class*/ {
          |    def <<scalameta>>/*method,declaration*/: <<String>>/*type*/
          |  } = new:
          |    def <<scalameta>>/*method,definition*/ = "4.0"
          |  <<V>>/*variable,readonly*/.<<scalameta>>/*method*/
          |end <<StructuralTypes>>/*class,definition*/
          |""".stripMargin
    )

  @Test def `vars` =
    check(
      s"""|package <<example>>/*namespace*/
          |
          |object <<A>>/*class*/ {
          |  val <<a>>/*variable,definition,readonly*/ = 1
          |  var <<b>>/*variable,definition*/ = 2
          |  val <<c>>/*variable,definition,readonly*/ = <<List>>/*class*/(1,<<a>>/*variable,readonly*/,<<b>>/*variable*/)
          |  <<b>>/*variable*/ = <<a>>/*variable,readonly*/
          |""".stripMargin
    )

  @Test def `predef` =
    check(
      """
        |object <<Main>>/*class*/ {
        |val <<a>>/*variable,definition,readonly*/ = <<List>>/*class*/(1,2,3)
        |val <<y>>/*variable,definition,readonly*/ = <<Vector>>/*class*/(1,2)
        |val <<z>>/*variable,definition,readonly*/ = <<Set>>/*class*/(1,2,3)
        |val <<w>>/*variable,definition,readonly*/ = <<Right>>/*class*/(1)
        |}""".stripMargin
    )

  @Test def `predef1` =
    check(
      """
        |object <<Main>>/*class*/ {
        |  val <<a>>/*variable,definition,readonly*/ = <<List>>/*class*/(1,2,3)
        |  val <<y>>/*class,definition*/ = <<List>>/*class*/
        |  val <<z>>/*class,definition*/ = <<scala>>/*namespace*/.<<collection>>/*namespace*/.<<immutable>>/*namespace*/.<<List>>/*class*/
        |}
        |""".stripMargin
    )

  @Test def `val-object` =
    check(
      """
        |case class <<X>>/*class*/(<<a>>/*variable,declaration,readonly*/: <<Int>>/*class,abstract*/)
        |object <<X>>/*class*/
        |
        |object <<Main>>/*class*/ {
        |  val <<x>>/*class,definition*/ = <<X>>/*class*/
        |  val <<y>>/*variable,definition,readonly*/ = <<X>>/*class*/(1)
        |}
        |""".stripMargin
    )

  @Test def `case-class` =
    check(
      """|case class <<Foo>>/*class*/(<<i>>/*variable,declaration,readonly*/: <<Int>>/*class,abstract*/, <<j>>/*variable,declaration,readonly*/: <<Int>>/*class,abstract*/)
         |
         |object <<A>>/*class*/ {
         |  val <<f>>/*variable,definition,readonly*/ = <<Foo>>/*class*/(1,2)
         |}
         |""".stripMargin
    )

  @Test def `import-selector` =
    check(
      """|package <<a>>/*namespace*/
         |
         |import <<a>>/*namespace*/.<<Tag>>/*class*/.<<@@>>/*type*/
         |
         |object <<A>>/*class*/ {
         |  case class <<B>>/*class*/(<<c>>/*variable,declaration,readonly*/: <<Int>>/*class,abstract*/)
         |}
         |
         |object <<Tag>>/*class*/ {
         |  type <<@@>>/*type,definition*/ = <<Int>>/*class,abstract*/
         |}
         |""".stripMargin
    )

  @Test def `main-annot` =
    check(
      """|@<<main>>/*class*/ def <<main>>/*method,definition*/(<<args>>/*parameter,declaration,readonly*/: <<Array>>/*class*/[<<String>>/*type*/]): <<Unit>>/*class,abstract*/ = ()
         |""".stripMargin
    )

  // When for-comprehension includes line with `=`, we get `scala.x$1`, `scala.x$2` symbols on `foo`.
  // Both `scala` and `x$#` have position on `foo`, and we don't want to highlight it as a `scala` package,
  // so we need `namespace` to have lower priority than `variable`.
  @Test def `for-comprehension` =
    check(
      """|package <<example>>/*namespace*/
         |
         |object <<B>>/*class*/ {
         |  val <<a>>/*variable,definition,readonly*/ = for {
         |    <<foo>>/*parameter,declaration,readonly*/ <- <<List>>/*class*/("a", "b", "c")
         |    <<_>>/*class,abstract*/ = <<println>>/*method*/("print!")
         |  } yield <<foo>>/*parameter,readonly*/
         |}
         |""".stripMargin
    )

  @Test def `named-arg-backtick` =
    check(
      """|object <<Main>>/*class*/ {
           |  def <<foo>>/*method,definition*/(<<`type`>>/*parameter,declaration,readonly*/: <<String>>/*type*/): <<String>>/*type*/ = <<`type`>>/*parameter,readonly*/
           |  val <<x>>/*variable,definition,readonly*/ = <<foo>>/*method*/(
           |    <<`type`>>/*parameter,readonly*/ = "abc"
           |  )
           |}
          |""".stripMargin
    )

  @Test def `end-marker` =
    check(
      """|def <<foo>>/*method,definition*/ =
           |  1
           |end <<foo>>/*method,definition*/
           |""".stripMargin
    )

  @Test def `constructor` =
    check(
      """
        |object <<Bar>>/*class*/ {
        |  class <<Abc>>/*class*/[<<T>>/*typeParameter,definition,abstract*/](<<a>>/*variable,declaration,readonly*/: <<T>>/*typeParameter,abstract*/)
        |}
        |
        |object <<O>>/*class*/ {
        |  val <<x>>/*variable,definition,readonly*/ = new <<Bar>>/*class*/.<<Abc>>/*class*/(2)
        |  val <<y>>/*variable,definition,readonly*/ = new <<Bar>>/*class*/.<<Abc>>/*class*/[<<Int>>/*class,abstract*/](2)
        |  val <<z>>/*variable,definition,readonly*/ = <<Bar>>/*class*/.<<Abc>>/*class*/(2)
        |  val <<w>>/*variable,definition,readonly*/ = <<Bar>>/*class*/.<<Abc>>/*class*/[<<Int>>/*class,abstract*/](2)
        |}""".stripMargin
    )

  @Test def `constructor1` =
    check(
      """
        |object <<Main>>/*class*/ {
        |  class <<Abc>>/*class*/[<<T>>/*typeParameter,definition,abstract*/](<<abc>>/*variable,declaration,readonly*/: <<T>>/*typeParameter,abstract*/)
        |  object <<Abc>>/*class*/
        |  val <<x>>/*variable,definition,readonly*/ = new <<Abc>>/*class*/(123)
        |}""".stripMargin
    )

  @Test def `constructor2` =
    check(
      """
        |object <<Main>>/*class*/ {
        |  class <<Abc>>/*class*/[<<T>>/*typeParameter,definition,abstract*/](<<abc>>/*variable,declaration,readonly*/: <<T>>/*typeParameter,abstract*/)
        |  object <<Abc>>/*class*/ {
        |    def <<apply>>/*method,definition*/[<<T>>/*typeParameter,definition,abstract*/](<<abc>>/*parameter,declaration,readonly*/: <<T>>/*typeParameter,abstract*/, <<bde>>/*parameter,declaration,readonly*/: <<T>>/*typeParameter,abstract*/) = new <<Abc>>/*class*/(<<abc>>/*parameter,readonly*/)
        |  }
        |  val <<x>>/*variable,definition,readonly*/ = <<Abc>>/*class*/(123, 456)
        |}""".stripMargin
    )

  @Test def `i5977` =
    check(
      """
        |sealed trait <<ExtensionProvider>>/*interface,abstract*/ {
        |  extension [<<A>>/*typeParameter,definition,abstract*/] (<<self>>/*parameter,declaration,readonly*/: <<A>>/*typeParameter,abstract*/) {
        |    def <<typeArg>>/*method,declaration*/[<<B>>/*typeParameter,definition,abstract*/]: <<B>>/*typeParameter,abstract*/
        |    def <<inferredTypeArg>>/*method,declaration*/[<<C>>/*typeParameter,definition,abstract*/](<<value>>/*parameter,declaration,readonly*/: <<C>>/*typeParameter,abstract*/): <<C>>/*typeParameter,abstract*/
        |}
        |
        |object <<Repro>>/*class*/ {
        |  def <<usage>>/*method,definition*/[<<A>>/*typeParameter,definition,abstract*/](<<f>>/*parameter,declaration,readonly*/: <<ExtensionProvider>>/*interface,abstract*/ ?=> <<A>>/*typeParameter,abstract*/ => <<Any>>/*class,abstract*/): <<Any>>/*class,abstract*/ = <<???>>/*method*/
        |
        |  <<usage>>/*method*/[<<Int>>/*class,abstract*/](<<_>>/*parameter,readonly*/.<<inferredTypeArg>>/*method*/("str"))
        |  <<usage>>/*method*/[<<Int>>/*class,abstract*/](<<_>>/*parameter,readonly*/.<<inferredTypeArg>>/*method*/[<<String>>/*type*/]("str"))
        |  <<usage>>/*method*/[<<Option>>/*class,abstract*/[<<Int>>/*class,abstract*/]](<<_>>/*parameter,readonly*/.<<typeArg>>/*method*/[<<Some>>/*class*/[<<Int>>/*class,abstract*/]].<<value>>/*variable,readonly*/.<<inferredTypeArg>>/*method*/("str"))
        |  <<usage>>/*method*/[<<Option>>/*class,abstract*/[<<Int>>/*class,abstract*/]](<<_>>/*parameter,readonly*/.<<typeArg>>/*method*/[<<Some>>/*class*/[<<Int>>/*class,abstract*/]].<<value>>/*variable,readonly*/.<<inferredTypeArg>>/*method*/[<<String>>/*type*/]("str"))
        |}
        |""".stripMargin
    )

  @Test def `local-object-with-end-i7246` =
    check(
      """|def <<bar>>/*method,definition*/ =
         |  object <<foo>>/*class*/:
         |    def <<aaa>>/*method,definition*/ = <<???>>/*method*/
         |  end <<foo>>/*class,definition*/
         |""".stripMargin
    )

  @Test def i7256 =
    check(
      """|object <<Test>>/*class*/:
         |  def <<methodA>>/*method,definition*/: <<Unit>>/*class,abstract*/ = <<???>>/*method*/
         |export <<Test>>/*class*/.<<methodA>>/*method*/
         |""".stripMargin
    )
