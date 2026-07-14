trait Context: // Dummy scala.reflect.macros.Context
  type Tree = universe.Tree
  val universe: Universe

  trait Universe {
    type Tree >: Null <: AnyRef & TreeApi
    type Literal >: Null <: LiteralApi & TermTree
    type TermTree >: Null <: TermTreeApi & Tree

    trait TermTreeApi extends TreeApi { this: TermTree => }
    trait LiteralApi extends TermTreeApi { this: Literal => }
    trait TreeApi extends Product { this: Tree => }

    type Constant

    type Type

    def Literal(const: Constant): Tree
    def Constant(i: Int): Constant
    def New(tpe: Type, arg: Tree): Tree
  }

  def enclosingPosition: Position

  trait Mirror {
    def staticClass(name: String): universe.Type
  }
  val mirror: Mirror

class Position(val line: Int)
