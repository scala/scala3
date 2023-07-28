import scala.quoted.*

def withType[T <: AnyKind, U](tpe: Type[T])(body: [X <: T] => Type[X] ?=> U)(using Quotes): U =
  type X <: T
  val tpeX: Type[X] = tpe.asInstanceOf[Type[X]]
  body[X](using tpeX)

def test1(t1: Type[?], t2: Type[? <: Any])(using Quotes) =
  withType(t1) { [T <: AnyKind] => _ ?=> // TODO remove _ ?=> // Implementation restriction: polymorphic function literals must have a value parameter
    Type.of[T]
    Type.show[T]
  }
  withType(t2) { [T] => _ ?=> // TODO remove _ ?=>
    '{ val a: T = ??? }
    Type.of[T]
    Type.show[T]
  }
  withType(t2):
    [T] => _ ?=> '{ val a: T = ??? } // TODO remove _ ?=>

def exprWithPreciseType[T, U](expr: Expr[T])(body: [X <: T] => Type[X] ?=> Expr[X] => U)(using Quotes): U =
  import quotes.reflect.*
  type X <: T
  val exprX = expr.asInstanceOf[Expr[X]]
  val tpeX = expr.asTerm.tpe.asType.asInstanceOf[Type[X]]
  body[X](using tpeX)(exprX)

def test2(x: Expr[Any])(using Quotes) =
  // exprWithPreciseType(x) { [T] => x => // Inference limitation: x is assumed to be the Type[T] instead of the Expr[T]
  exprWithPreciseType(x) { [T] => _ ?=> x => // TODO remove _ ?=>
    Type.of[T]
    '{ val a: T = $x }
  }
  exprWithPreciseType('{1}) { [T <: Int] => _ ?=> x => // TODO remove _ ?=>
    Type.of[T]
    '{ val a: Int = $x }
    '{ val a: T = $x }
    '{ val a: T = i($x) }
  }

def i[T <: Int](x: T): T = x
