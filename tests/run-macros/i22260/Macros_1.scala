import scala.quoted.*

object Macros:
  inline def crMethod: Int = ${ createMethod }
  inline def inMethod: Int = ${ insideMethod }
  inline def usMethod: Int = ${ usingMethod }
  inline def inObject: Int = ${ insideObject }
  inline def inClass: Int = ${ insideClass }
  inline def usClass: Int = ${ usingClass }

  def summon(using Quotes) =
    Expr.summon[Int].getOrElse('{0})

  def createMethod(using Quotes): Expr[Int] =
    import quotes.reflect.*
    val tpe = MethodType(MethodTypeKind.Contextual)("x" :: Nil)(_ => TypeRepr.of[Int] :: Nil, _ => TypeRepr.of[Int])
    val sym = Symbol.newMethod(Symbol.spliceOwner, "foo", tpe)
    val method = DefDef(sym, _ => Some(summon(using sym.asQuotes).asTerm))
    Block(method :: Nil, Apply(Ref(sym), '{42}.asTerm :: Nil)).asExprOf[Int]

  def insideMethod(using Quotes): Expr[Int] = '{
    def foo =
      given Int = 42
      ${summon}

    foo
  }

  def usingMethod(using Quotes): Expr[Int] = '{
    def foo(using Int) =
      ${summon}

    foo(using 42)
  }

  def insideObject(using Quotes): Expr[Int] = '{
    object Foo:
      given Int = 42
      val x = ${summon}

    Foo.x
  }

  def insideClass(using Quotes): Expr[Int] = '{
    class Foo:
      given Int = 42
      val x = ${summon}

    new Foo().x
  }

  def usingClass(using Quotes): Expr[Int] = '{
    class Foo(using Int):
      val x = ${summon}

    new Foo(using 42).x
  }
