//> using options -experimental -Yno-experimental

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
      // Without TypeRep.of[Object] it would fail with java.lang.AssertionError: assertion failed: First parent must be a class
      List(TypeRepr.of[Object], TypeRepr.of[Serializer[T]]),
      _ => Nil,
      Symbol.noSymbol
    )

    val (modValDef: ValDef, modClassDef: ClassDef) =
      ClassDef.module(modSym, List(TypeTree.of[Serializer[T]]), Nil)

    Block(List(modValDef, modClassDef), Ref(modSym)).asExprOf[Serializer[T]]
  }
}