trait Statement
trait Definition extends Statement

trait ClassDef extends Definition:
  def constructor: DefDef

object ClassDef:
  def copy(constr: DefDef): ClassDef = ???

// >>> This abstract implementation of DefDef causes a compilation error in transform...
type DefDef <: Definition
val DefDef: DefDefModule = ???
trait DefDefModule:
  def unapply(ddef: DefDef): (String, List[AnyRef])
// ...unless this given TypeTest is commented out, in which case we get only a type test warning
given scala.reflect.TypeTest[Statement, DefDef] = ???

// >>> This alternative works
// trait DefDef extends Definition
// object DefDef:
//   def unapply(ddef: DefDef): (String, List[AnyRef]) = ???

// >>> This alternative also works
// case class DefDef(name: String, paramss: List[AnyRef]) extends Definition

def transform(tree: Statement): Statement = tree match
  case tree: ClassDef =>
    val constructor @ DefDef(_, _) = transform(tree.constructor): @unchecked
    ClassDef.copy(constructor)
