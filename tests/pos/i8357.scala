trait FieldInfo {
  val fieldType: ALL_TYPE
}

opaque type TypeSymbol = String

trait ConcreteType {
  val name: String
  val typeParameters: List[TypeSymbol]
  val isUnion: Boolean = false
}

trait UnionContainer {
  val hasUnion: Boolean
}

type ALL_TYPE = ConcreteType | TypeSymbol

case class StaticUnionInfo(name: String, typeParameters: List[TypeSymbol]) extends ConcreteType 
case class AliasInfo(name: String, typeParameters: List[TypeSymbol]) extends ConcreteType

object UnionKind {
  def unapply(f: FieldInfo): Boolean = 
    f.fieldType match {
      case _: StaticUnionInfo => true
      case t: AliasInfo if t.isUnion =>  true
      case t: UnionContainer if t.hasUnion => true  // this line here causes crash
      case _ => false
    }
}