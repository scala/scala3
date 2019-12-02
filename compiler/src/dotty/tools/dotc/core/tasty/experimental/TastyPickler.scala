package dotty.tools.dotc.core.tasty.experimental

import dotty.tools.dotc.core.Symbols.ClassSymbol

import dotty.tools.tasty.experimental.{TastyPickler => TastyPicklerImpl}

class TastyPickler(rootCls: ClassSymbol) extends TastyPicklerImpl[DottyTasty.type](DottyTasty)(rootCls)
