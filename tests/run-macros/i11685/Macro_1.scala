//> using options -experimental

package test

import scala.quoted.*

trait Thing {
  type Type
}

object MyMacro {

  def isExpectedReturnType[R: Type](using Quotes): quotes.reflect.Symbol => Boolean = { method =>
    import quotes.reflect.*

    val expectedReturnType = TypeRepr.of[R]

    method.tree match {
      case DefDef(_,_,typedTree,_) =>
          TypeRepr.of(using typedTree.tpe.asType) <:< expectedReturnType
      case _ => false
    }
  }

  ///TODO no overloads
  def checkMethod[R: Type](using q: Quotes)(method: quotes.reflect.Symbol): Option[String] = {
    val isExpectedReturnTypeFun = isExpectedReturnType[R]

    Option.when(method.paramSymss.headOption.exists(_.exists(_.isType)))(s"Method ${method.name} has a generic type parameter, this is not supported") orElse
      Option.when(!isExpectedReturnTypeFun(method))(s"Method ${method.name} has unexpected return type")
  }

  def definedMethodsInType[T: Type](using Quotes): List[quotes.reflect.Symbol] = {
    import quotes.reflect.*

    val tree = TypeTree.of[T]

    for {
      member <- tree.symbol.methodMembers
      //is abstract method, not implemented
      if member.flags.is(Flags.Deferred)

      //TODO: is that public?
      // TODO? if member.privateWithin
      if !member.flags.is(Flags.Private)
      if !member.flags.is(Flags.Protected)
      if !member.flags.is(Flags.PrivateLocal)

      if !member.isClassConstructor
      if !member.flags.is(Flags.Synthetic)
    } yield member
  }

  transparent inline def client[T, R](r: () => R): T = ${MyMacro.clientImpl[T, R]('r)}

  def clientImpl[T: Type, R: Type](r: Expr[() => R])(using Quotes): Expr[T] = {
    import quotes.reflect.*

    val apiType = TypeRepr.of[T]
    val tree = TypeTree.of[T]

    val methods = definedMethodsInType[T]
    val invalidMethods = methods.flatMap(checkMethod[R])
    if (invalidMethods.nonEmpty) {
      report.errorAndAbort(s"Invalid methods: ${invalidMethods.mkString(", ")}")
    }

    val className = "_Anon"
    val parents = List(TypeTree.of[Object], TypeTree.of[T])

    def decls(cls: Symbol): List[Symbol] = methods.map { method =>
      Symbol.newMethod(cls, method.name, method.info, flags = Flags.EmptyFlags /*TODO: method.flags */, privateWithin = method.privateWithin.fold(Symbol.noSymbol)(_.typeSymbol))
    }

    val cls = Symbol.newClass(Symbol.spliceOwner, className, parents = parents.map(_.tpe), decls, selfType = None)
    val body = cls.declaredMethods.map { method => DefDef(method, argss => Some('{${r}()}.asTerm)) }
    val clsDef = ClassDef(cls, parents, body = body)
    val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[T])
    Block(List(clsDef), newCls).asExprOf[T]
  }
}
