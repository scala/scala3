package dotty.tools.dotc.core

import dotty.tools.dotc.core.NameOps.{isOperatorName, moduleClassName, stripModuleClassSuffix}
import dotty.tools.dotc.core.Names.{termName, typeName, SimpleName}
import dotty.tools.dotc.core.StdNames.nme

import org.junit.Assert.*
import org.junit.Test

class NameOpsTest:
  @Test def isOperatorNamePos: Unit =
    for name <- List("+", "::", "frozen_=:=", "$_+", "a2_+", "a_b_+") do
      assert(isOperatorName(termName(name)))

  @Test def isOperatorNameNeg: Unit =
    for name <- List("foo", "*_*", "<init>", "$reserved", "a*", "2*") do
      assert(!isOperatorName(termName(name)))

  @Test def stripModuleClassSuffixNoOpPreservesSimpleNames: Unit =
    val term = termName("ordinary")
    assertSame(term, term.stripModuleClassSuffix)

    val tpe = typeName("Ordinary")
    assertSame(tpe, tpe.stripModuleClassSuffix)

  @Test def stripModuleClassSuffixStillHandlesModuleNames: Unit =
    assertEquals(termName("Foo"), termName("Foo$").stripModuleClassSuffix)
    assertEquals(typeName("Foo"), termName("Foo").moduleClassName.stripModuleClassSuffix)
    assertSame(nme.nothingClass, nme.nothingClass.stripModuleClassSuffix)
