package dotty.tools.scaladoc
package tasty.comments

import scala.quoted.*

import org.junit.{Test, Rule}
import org.junit.Assert.{assertSame, assertTrue}
import dotty.tools.scaladoc.tasty.util._

class LookupTestCases[Q <: Quotes](val q: Quotes) {

  given DocContext = testDocContext()

  def testAll(): Unit = {
    testOwnerlessLookup()
    testOwnerlessLookupOfInherited()
    testOwnerlessLookupOfClassWithinPackageWithPackageObject()
    testOwnedLookup()
    testStrictMemberLookup()
    testOverloadedMethodLookup()
  }

  def testOwnerlessLookup(): Unit = {
    val cases = List[(String, Sym)](
      "Array" -> cls("scala.Array"),
      "Option" -> cls("scala.Option"),
      "Predef$" -> cls("scala.Predef$"),
      "Predef$.identity" -> cls("scala.Predef$").fun("identity"),
      "Predef.identity" -> cls("scala.Predef$").fun("identity"),
      "Array$.from" -> cls("scala.Array$").fun("from"),
      "???" -> cls("scala.Predef$").fun("???"),
      "scala.List" -> cls("scala.package$").tpe("List"),

      "scala.List.lift" -> cls("scala.PartialFunction").fun("lift"),

      "tests.A" -> cls("tests.A"),
      "tests.A$" -> cls("tests.A$"),
      "tests.Methods.simple" -> cls("tests.Methods").fun("simple"),
      "tests.foo" -> cls("tests.package$").fld("foo"),
      "tests.bar" -> cls("tests.tests$package$").fld("bar"),

      "java.util.AbstractCollection" -> cls("java.util.AbstractCollection"),
      "java.lang.String" -> cls("java.lang.String"),
      "java.util.Formatter" -> cls("java.util.Formatter"),
      "java.io.Flushable" -> cls("java.io.Flushable"),
      "java.util.List" -> cls("java.util.List"),

      "tests.lookupInheritedMembers.pack1.A.x" ->
        cls("tests.lookupInheritedMembers.pack1.A").fun("x"),

      "tests.lookupInheritedMembers.pack2.B.x" ->
        cls("tests.lookupInheritedMembers.pack2.B").fun("x"),
    )

    cases.foreach { case (query, sym) =>
      testOwnerlessLookup(query, sym)
    }
  }

  def testOwnerlessLookup(query: String, wrappedTarget: Sym): Unit = {
    val target = wrappedTarget.symbol
    val lookupRes = MemberLookup.lookupOpt(parseQuery(query), None)
    assertTrue(s"Couldn't look up: $query", lookupRes.nonEmpty)
    val Some((lookedUp, _, _)) = lookupRes: @unchecked
    assertSame(query, target, lookedUp)
  }

  /**
   * We cannot test for cls().fun() beucase it returns parent fun symbol from tasty. Hence we will look for member (val, def, type) but compare its owner to just cls()
   */
  def testOwnerlessLookupOfInherited(): Unit = {
    val cases = List[(String, Sym)](
      "tests.lookupInheritedMembers.pack2.B.x" ->
        cls("tests.lookupInheritedMembers.pack2.B"),

      "tests.lookupInheritedMembers.pack2.B.y" ->
        cls("tests.lookupInheritedMembers.pack2.B"),

      "tests.lookupInheritedMembers.pack2.B.MyType" ->
        cls("tests.lookupInheritedMembers.pack2.B"),
    )

    cases.foreach { case (query, sym) =>
      val target = sym.symbol
      val lookupRes = MemberLookup.lookupOpt(parseQuery(query), None)
      assertTrue(s"Couldn't look up: $query", lookupRes.nonEmpty)
      val Some((_ , _, Some(owner))) = lookupRes: @unchecked
      assertSame(query, target, owner)
    }
  }

  /**
   * Classes should not have owner of package object
   */
  def testOwnerlessLookupOfClassWithinPackageWithPackageObject(): Unit = {
    val cases = List[(String, Sym)](
      "<:<" ->
        cls("scala.<:<"),
    )

    cases.foreach { case (query, sym) =>
      val target = sym.symbol
      val lookupRes = MemberLookup.lookupOpt(parseQuery(query), Some(cls("scala.=:=").symbol))
      assertTrue(s"Couldn't look up: $query", lookupRes.nonEmpty)
      println(lookupRes)
      val Some((_ , _, owner)) = lookupRes: @unchecked
      assertSame(query, None, owner)
    }
  }

  def testOwnedLookup(): Unit = {
    val cases = List[((Sym, String), Sym)](
      cls("tests.A") -> "tests.Methods.simple" -> cls("tests.Methods").fun("simple"),
      cls("tests.A") -> "tests#Methods#simple" -> cls("tests.Methods").fun("simple"),

      cls("tests.A") -> "method" -> cls("tests.A").fun("method"),
      cls("tests.A") -> "#method" -> cls("tests.A").fun("method"),
      cls("tests.A") -> "method*" -> cls("tests.A").fun("method"),
      cls("tests.A") -> "method[T]*" -> cls("tests.A").fun("method"),
      cls("tests.A") -> "method(str:String*" -> cls("tests.A").fun("method"),

      cls("tests.A") -> "tests.B" -> cls("tests.B"),
      cls("tests.A") -> "tests.B$" -> cls("tests.B$"),

      cls("tests.A") -> "AA" -> cls("tests.A").tpe("AA"),
      cls("tests.A") -> "#AA" -> cls("tests.A").tpe("AA"),
      cls("tests.A") -> "AA!" -> cls("tests.A").tpe("AA"),
      cls("tests.A") -> "AA$" -> cls("tests.A").fld("AA"),

      cls("tests.C") -> "CC" -> cls("tests.C").tpe("CC"),
      cls("tests.C") -> "CC$" -> cls("tests.C").fld("CC"),
      cls("tests.C") -> "CC!" -> cls("tests.C").tpe("CC"),

      cls("tests.A").fun("method") -> "AA" -> cls("tests.A").tpe("AA"),
      cls("tests.A").fun("method") -> "AA!" -> cls("tests.A").tpe("AA"),
      cls("tests.A").fun("method") -> "AA$" -> cls("tests.A").fld("AA"),

      cls("tests.Methods").fun("simple") -> "generic" -> cls("tests.Methods").fun("generic"),
      cls("tests.Methods").fun("simple") -> "#generic" -> cls("tests.Methods").fun("generic"),

      cls("tests.A").fun("method") -> "B" -> cls("tests.B"),
      cls("tests.A").fun("method") -> "B$" -> cls("tests.B$"),

      cls("tests.A") -> "B.method" -> cls("tests.B").fun("method"),
      cls("tests.A") -> "Option" -> cls("scala.Option"),

      /*sanity*/ cls("tests.A") -> "this.X" -> cls("tests.A").tpe("X"),
      /*sanity*/ cls("tests.A") -> "this.Y" -> cls("tests.A").tpe("Y"),
      cls("tests.A") -> "this.X.method" -> cls("tests.B").fun("method"),
      cls("tests.A") -> "this.Y.method" -> cls("tests.B").fun("method"),

      cls("tests.A") -> "A.foo" -> cls("tests.A$").fun("foo"),

      cls("tests.inner.B") -> "A" -> cls("tests.inner.A$"),

      cls("tests.B$") -> "foo" -> cls("tests.BModule").fun("foo"),

      cls("tests.D") -> "foo" -> cls("tests.package$").fld("foo"),
      cls("tests.D") -> "bar" -> cls("tests.tests$package$").fld("bar"),
      cls("tests.inner.A$") -> "foo" -> cls("tests.package$").fld("foo"),
      cls("tests.inner.A$") -> "bar" -> cls("tests.tests$package$").fld("bar"),
    )

    cases.foreach { case ((Sym(owner), query), Sym(target)) =>
      val Some((lookedUp, _, _)) = MemberLookup.lookup(parseQuery(query), owner): @unchecked
      assertSame(s"$owner / $query", target, lookedUp)
    }
  }

  def testStrictMemberLookup(): Unit = {
    val owner = cls("tests.A").symbol
    val query = "#A"

    assertTrue("strict member lookup should not look outside", MemberLookup.lookup(parseQuery(query), owner).isEmpty)
  }

  def testOverloadedMethodLookup(): Unit = {
    // Test basic overload resolution with collection types
    val collectionOverloadCases = List[(String, Sym)](
      // Buffer overload
      "tests.overloadedMethods.OverloadedMethods.processBuffer[A](b:scala.collection.mutable.Buffer[A])*" ->
        cls("tests.overloadedMethods.OverloadedMethods").funOverload("processBuffer", "Buffer"),
      // Map overload
      "tests.overloadedMethods.OverloadedMethods.processMap[K,V](m:scala.collection.mutable.Map[K,V])*" ->
        cls("tests.overloadedMethods.OverloadedMethods").funOverload("processMap", "Map"),
      // Set overload
      "tests.overloadedMethods.OverloadedMethods.processSet[A](s:scala.collection.mutable.Set[A])*" ->
        cls("tests.overloadedMethods.OverloadedMethods").funOverload("processSet", "Set"),
      // Seq overload
      "tests.overloadedMethods.OverloadedMethods.processSeq[A](s:scala.collection.mutable.Seq[A])*" ->
        cls("tests.overloadedMethods.OverloadedMethods").funOverload("processSeq", "Seq"),
      // Iterator overload
      "tests.overloadedMethods.OverloadedMethods.processIterator[A](it:scala.collection.Iterator[A])*" ->
        cls("tests.overloadedMethods.OverloadedMethods").funOverload("processIterator", "Iterator"),
      // Iterable overload
      "tests.overloadedMethods.OverloadedMethods.processIterable[A](it:scala.collection.mutable.Iterable[A])*" ->
        cls("tests.overloadedMethods.OverloadedMethods").funOverload("processIterable", "Iterable"),
      // List overload
      "tests.overloadedMethods.OverloadedMethods.processList[A](l:scala.collection.immutable.List[A])*" ->
        cls("tests.overloadedMethods.OverloadedMethods").funOverload("processList", "List"),
      // Properties overload
      "tests.overloadedMethods.OverloadedMethods.processProperties(p:java.util.Properties)*" ->
        cls("tests.overloadedMethods.OverloadedMethods").funOverload("processProperties", "Properties"),
    )

    collectionOverloadCases.foreach { case (query, sym) =>
      testOwnerlessLookup(query, sym)
    }

    // Test transform method with multiple overloads including primitive Int
    val transformOverloadCases = List[(String, Sym)](
      // Int overload (primitive type)
      "tests.overloadedMethods.OverloadedMethods.transform(x:Int)*" ->
        cls("tests.overloadedMethods.OverloadedMethods").funOverload("transform", "Int"),
      // Buffer overload
      "tests.overloadedMethods.OverloadedMethods.transform[A](b:scala.collection.mutable.Buffer[A])*" ->
        cls("tests.overloadedMethods.OverloadedMethods").funOverload("transform", "Buffer"),
      // Map overload
      "tests.overloadedMethods.OverloadedMethods.transform[K,V](m:scala.collection.mutable.Map[K,V])*" ->
        cls("tests.overloadedMethods.OverloadedMethods").funOverload("transform", "Map"),
      // Set overload
      "tests.overloadedMethods.OverloadedMethods.transform[A](s:scala.collection.mutable.Set[A])*" ->
        cls("tests.overloadedMethods.OverloadedMethods").funOverload("transform", "Set"),
      // Properties overload
      "tests.overloadedMethods.OverloadedMethods.transform(p:java.util.Properties)*" ->
        cls("tests.overloadedMethods.OverloadedMethods").funOverload("transform", "Properties"),
    )

    transformOverloadCases.foreach { case (query, sym) =>
      testOwnerlessLookup(query, sym)
    }

    // Test static asJava methods in companion object
    val staticOverloadCases = List[(String, Sym)](
      // Buffer overload
      "tests.overloadedMethods.OverloadedMethods.asJava[A](b:scala.collection.mutable.Buffer[A])*" ->
        cls("tests.overloadedMethods.OverloadedMethods$").funOverload("asJava", "Buffer"),
      // Map overload
      "tests.overloadedMethods.OverloadedMethods.asJava[K,V](m:scala.collection.mutable.Map[K,V])*" ->
        cls("tests.overloadedMethods.OverloadedMethods$").funOverload("asJava", "Map"),
      // Set overload
      "tests.overloadedMethods.OverloadedMethods.asJava[A](s:scala.collection.mutable.Set[A])*" ->
        cls("tests.overloadedMethods.OverloadedMethods$").funOverload("asJava", "Set"),
      // Properties overload
      "tests.overloadedMethods.OverloadedMethods.asJava(p:java.util.Properties)*" ->
        cls("tests.overloadedMethods.OverloadedMethods$").funOverload("asJava", "Properties"),
    )

    staticOverloadCases.foreach { case (query, sym) =>
      testOwnerlessLookup(query, sym)
    }

    // Test custom types (non-collection)
    val customTypeCases = List[(String, Sym)](
      "tests.overloadedMethods.CustomTypeOverloads.process(x:tests.overloadedMethods.CustomTypeA)*" ->
        cls("tests.overloadedMethods.CustomTypeOverloads").funOverload("process", "CustomTypeA"),
      "tests.overloadedMethods.CustomTypeOverloads.process(x:tests.overloadedMethods.CustomTypeB)*" ->
        cls("tests.overloadedMethods.CustomTypeOverloads").funOverload("process", "CustomTypeB"),
    )

    customTypeCases.foreach { case (query, sym) =>
      testOwnerlessLookup(query, sym)
    }

    // Test multiple parameters
    val multiParamCases = List[(String, Sym)](
      "tests.overloadedMethods.MultiParamOverloads.merge(a:scala.collection.mutable.Buffer[_],b:scala.collection.mutable.Set[_])*" ->
        cls("tests.overloadedMethods.MultiParamOverloads").funOverload("merge", "Buffer", "Set"),
      "tests.overloadedMethods.MultiParamOverloads.merge(a:scala.collection.mutable.Set[_],b:scala.collection.mutable.Buffer[_])*" ->
        cls("tests.overloadedMethods.MultiParamOverloads").funOverload("merge", "Set", "Buffer"),
      "tests.overloadedMethods.MultiParamOverloads.combine(a:Int,b:String)*" ->
        cls("tests.overloadedMethods.MultiParamOverloads").funOverload("combine", "Int", "String"),
      "tests.overloadedMethods.MultiParamOverloads.combine(a:String,b:Int)*" ->
        cls("tests.overloadedMethods.MultiParamOverloads").funOverload("combine", "String", "Int"),
    )

    multiParamCases.foreach { case (query, sym) =>
      testOwnerlessLookup(query, sym)
    }

    // Test fallback behavior - queries without signature should fall back to first match
    val fallbackCases = List[String](
      "tests.overloadedMethods.OverloadedMethods.transform",
      "tests.overloadedMethods.OverloadedMethods.processBuffer",
    )

    fallbackCases.foreach { query =>
      val lookupRes = MemberLookup.lookupOpt(parseQuery(query), None)
      assertTrue(s"Fallback lookup should succeed for: $query", lookupRes.nonEmpty)
    }
  }

  given q.type = q

  def parseQuery(query: String): Query = {
    val Right(parsed) = QueryParser(query).tryReadQuery(): @unchecked
    parsed
  }

  case class Sym(symbol: q.reflect.Symbol) {
    def fld(name: String) =
      def hackResolveModule(s: q.reflect.Symbol): q.reflect.Symbol =
        if s.flags.is(q.reflect.Flags.Module) then s.moduleClass else s
      Sym(hackResolveModule(symbol.declaredField(name)))
    def fun(name: String) =
      val List(sym) = symbol.methodMember(name)
      Sym(sym)
    def tpe(name: String) = Sym(symbol.typeMember(name))
    /** Find a specific overloaded method by matching parameter types.
      *  @param name The method name
      *  @param paramTypes The simple names of parameter types to match (e.g., "Buffer", "Int")
      */
    def funOverload(name: String, paramTypes: String*): Sym = {
      import q.reflect._

      def extractSimpleTypeName(qualifiedType: String): String = {
        val withoutTypeArgs = qualifiedType.indexOf('[') match {
          case -1 => qualifiedType
          case i => qualifiedType.substring(0, i)
        }
        withoutTypeArgs.split('.').last
      }

      def getMethodParamTypes(tpe: TypeRepr): Option[List[String]] = tpe match {
        case MethodType(_, pts, _) =>
          Some(pts.map(pt => extractSimpleTypeName(pt.show)))
        case PolyType(_, _, resType) =>
          getMethodParamTypes(resType)
        case _ => None
      }

      val methods = symbol.methodMember(name)
      val targetTypes = paramTypes.toList
      val matchingMethod = methods.find { sym =>
        getMethodParamTypes(sym.info) match {
          case Some(actualTypes) => actualTypes == targetTypes
          case None => false
        }
      }
      matchingMethod match {
        case Some(sym) => Sym(sym)
        case None =>
          throw new AssertionError(
            s"Could not find overload $name(${paramTypes.mkString(", ")}) in ${symbol.fullName}. " +
            s"Available overloads: ${methods.map(m => m.name + ": " + m.info.show).mkString("; ")}"
          )
      }
    }
  }

  def cls(fqn: String) = Sym(q.reflect.Symbol.classSymbol(fqn))
}

class MemberLookupTests {

  @Test
  def test(): Unit = {
    import scala.tasty.inspector.*
    class MyInspector extends Inspector:

      def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
        this.test()

      def test()(using q: Quotes): Unit = {
        import dotty.tools.scaladoc.tasty.comments.MemberLookup

        val cases = LookupTestCases[q.type](q)

        cases.testAll()
      }

    TastyInspector.inspectTastyFiles(TestUtils.listOurClasses())(new MyInspector)
  }
}
