package dotty.tools.dotc.core

import dotty.tools.dotc.core.NameOps.isOperatorName
import dotty.tools.dotc.core.Names.{termName, SimpleName}

import org.junit.Test

class NameOpsTest:
  @Test def isOperatorNamePos: Unit =
    for name <- List("+", "::", "frozen_=:=", "$_+", "a2_+", "a_b_+") do
      assert(isOperatorName(termName(name)))

  @Test def isOperatorNameNeg: Unit =
    for name <- List("foo", "*_*", "<init>", "$reserved", "a*", "2*") do
      assert(!isOperatorName(termName(name)))
