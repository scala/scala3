package dotty.tools
package dotc

import core.*, Contexts.*, Decorators.*, Denotations.*, Flags.*, Names.*, StdNames.*, SymDenotations.*, Symbols.*, Types.*
import config.Printers.*
import printing.Formatting.Show

import org.junit.Test
import org.junit.Assert.*

class StringFormatterTest extends AbstractStringFormatterTest:
  @Test def string     = check("foo", i"${"foo"}")
  @Test def integer    = check("1", i"${Int.box(1)}")
  @Test def type1      = check("Any", i"${defn.AnyType}")
  @Test def symbol     = check("class Any", i"${defn.AnyClass}")
  @Test def paramInfo  = check("class Any", i"${defn.AnyClass: ParamInfo}")
  @Test def seq        = check("[Any, String]", i"${Seq(defn.AnyType, defn.StringType)}")
  @Test def seqSep     = check("Any; String", i"${Seq(defn.AnyType, defn.StringType)}%; %")
  @Test def tuple      = check("(1,Any)", i"${(1, defn.AnyType)}")
  @Test def seqOfTup   = check("(1,Any), (2,String)", i"${Seq(1 -> defn.AnyType, 2 -> defn.StringType)}%, %")
  @Test def flags1     = check("final", i"$Final")
  @Test def flagsSeq   = check("<static>, final", i"${Seq(JavaStatic, Final)}%, %")
  @Test def flagsTup   = check("(<static>,final)", i"${(JavaStatic, Final)}")
  @Test def seqOfTup2  = check("(final,given), (private,lazy)", i"${Seq((Final, Given), (Private, Lazy))}%, %")
  @Test def seqOfTup3  = check("(Foo,given, (right is approximated))", i"${Seq((Foo, Given, TypeComparer.ApproxState.None.addHigh))}%, %")
  @Test def tupleNull   = check("(1,null)", i"${(1, null: String | Null)}")

  class StorePrinter extends Printer:
    var string: String = "<never set>"
    override def println(msg: => String) = string = msg

  @Test def testShowing: Unit =
    val store = StorePrinter()
    (JavaStatic | Final).showing(i"flags=$result", store)
    assertEquals("flags=final <static>", store.string)

  @Test def testShowingWithOriginalType: Unit =
    val store = StorePrinter()
    (JavaStatic | Final).showing(i"flags=${if result.is(Private) then result &~ Private else result | Private}", store)
    assertEquals("flags=private final <static>", store.string)
end StringFormatterTest

abstract class AbstractStringFormatterTest extends DottyTest:
  override def initializeCtx(fc: FreshContext) = super.initializeCtx(fc.setSetting(fc.settings.color, "never"))

  def Foo = newSymbol(defn.RootClass, typeName("Foo"), EmptyFlags, TypeBounds.empty).typeRef
  def Err = newErrorSymbol(defn.RootClass, typeName("Err"), "".toMessage)
  def Big = (1 to 120).foldLeft(defn.StringType)((tp, i) => RefinedType(tp, typeName("A" * 69 + i), TypeAlias(defn.IntType)))

  def mkCstrd =
    val names = List(typeName("Foo"), typeName("Bar"))
    val infos = List(TypeBounds.upper(defn.IntType), TypeBounds.upper(defn.StringType))
    val tl = PolyType(names)(_ => infos, _ => defn.AnyType)
    TypeComparer.addToConstraint(tl, Nil)
    tl.paramRefs

  def ckSub(obtained: String, snippet: String)  = assert(obtained.contains(snippet))
  def check(expected: String, obtained: String) = assertEquals(expected, obtained)
