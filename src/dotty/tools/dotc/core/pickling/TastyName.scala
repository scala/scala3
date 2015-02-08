package dotty.tools
package dotc
package core
package pickling

import core.Names.TermName

abstract class TastyName

object TastyName {
  
  class Ref(val index: Int) extends AnyVal
  
  case class Simple(name: TermName) extends TastyName
  case class Qualified(qualified: Ref, selector: Ref) extends TastyName
  case class Signed(original: Ref, params: List[Ref], result: Ref) extends TastyName 
  case class Expanded(original: Ref) extends TastyName
  case class ModuleClass(module: Ref) extends TastyName
  case class SuperAccessor(accessed: Ref) extends TastyName
  case class DefaultGetter(method: Ref, num: Int) extends TastyName
  
}  
