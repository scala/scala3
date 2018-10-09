package dotty.tools.dotc.quoted

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.util.NoSource

import scala.quoted.Type

/* Compilation unit containing the contents of a quoted type */
class TypeCompilationUnit(val tpe: Type[_]) extends CompilationUnit(NoSource) {
  override def toString: String = s"Type($tpe)"
}
