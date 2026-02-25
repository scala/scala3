import scala.annotation.{experimental, targetName}
import scala.quoted.*
import scala.util.Try

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
      def bye: (ValDef, ClassDef) = ???
      try {
        ClassDef.module(modSym, List(TypeTree.of[Serializer[T]]), Nil)
        assert(false, "Expected AssertionError")
      }
      catch {
        case ae: AssertionError
          if ae.getMessage.contains("Parents of class symbol differs from the parents in the tree")
        =>
          throw RuntimeException(ae.getMessage)
      }
      bye

    Block(List(modValDef, modClassDef), Ref(modSym)).asExprOf[Serializer[T]]
  }
}
