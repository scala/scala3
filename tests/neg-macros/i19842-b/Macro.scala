import scala.annotation.{experimental, targetName}
import scala.quoted.*
import scala.util.Try

trait Foo

object Macros {
  def makeSerializer[T: Type](using Quotes): Expr[Serializer[T]] = {
    import quotes.reflect.*

    val tpe: TypeRepr = TypeRepr.of[T]
    val name: String = Symbol.freshName("objectSerializer")

    val modSym: Symbol = Symbol.newModule(
      Symbol.spliceOwner,
      name,
      Flags.Implicit,
      Flags.EmptyFlags,
      _ => List(TypeRepr.of[Object], TypeRepr.of[Serializer[T]]),
      _ => Nil,
      Symbol.noSymbol
    )

    val (modValDef: ValDef, modClassDef: ClassDef) =
      ClassDef.module(modSym, List(TypeTree.of[Object], TypeTree.of[Serializer[T]], TypeTree.of[Foo]), Nil)

    Block(List(modValDef, modClassDef), Ref(modSym)).asExprOf[Serializer[T]]
  }
}