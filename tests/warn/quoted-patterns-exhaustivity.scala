import scala.quoted.*

def call() = 0
def foo(using Quotes) =

  val x: Type[Int] = ???
  val hkt: Type[List] = ???
  val anyExpr: Expr[Any] = '{0}

  val '{$t0} = '{0} // ok
  val '{$t1: Int} = '{0} // ok
  val '{$t2: t} = '{0} // ok
  val '{call(); $t3: tpe} = '{0} // warn
  val '[t] = x // ok
  val '[type t <: String; t] = x // warn
  val '[List[t]] = x // warn
  val '[t] = hkt // warn

  // wrapped in other types
  ('{1}, '{0}) match // ok
    case ('{$y}, '{$z}) => ()

  ('{1}, '{0}) match // ok
    case ('{$y: Int}, '{$z}) => ()

  ('{1}, '{""}) match // warn
    case ('{$y}, '{$z: Int}) => ()

  ('{0}, '{1}) match // warn
    case ('{0}, '{1}) => ()

  ('{0}, '{1}) match // warn
    case ('{call(); $y}, '{$z}) => ()

  ('{1}, x) match // ok
    case ('{$y}, '[t]) => ()

  ('{1}, x) match // ok
    case ('{$y: t}, '[q]) => ()

  (x, x) match // ok
    case ('[t], '[q]) => ()

  (x, x) match // warn
    case ('[type t <: Number; t], '[q]) => ()

  (x, x) match // warn
    case ('[List[t]], '[q]) => ()

  (x, x) match // warn
    case ('[List[String]], '[q]) => ()

  // individual
  '{1} match // ok
    case '{$y} => ()

  '{1} match // ok
    case '{$y: Int} => ()

  '{1} match // ok
    case '{$y: t} => ()

  '{""} match // warn
    case '{$z: Int} => ()

  '{0} match // warn
    case '{0} => ()

  '{0} match // warn
    case '{call(); $y} => ()

  x match // ok
    case '[t] => ()

  x match // warn
    case '[type t <: Number; t] => ()

  x match // warn
    case '[List[t]] => ()

  x match // warn
    case '[List[String]] => ()

  x match // warn
    case '[Int] => ()

  hkt match // warn
    case '[t] => ()

  anyExpr match // ok
    case '{ $x: t } => ()

  anyExpr match // warn
    case '{ type t <: String; $x: t } => ()

  val t = Type.of[Int]
  Type.of[Int] match // ok
    case '[t.Underlying] => ()

  // nested (causes a different shape of QuotePattern)
  def f[A: Type](e: Expr[A]): Expr[A] =
    e match // ok
      case '{ $e2 } => e2

  // Intersection types
  val tdx: Type[Int] & Serializable = ???
  Type.of[Int] match // ok
    case '[tdx.Underlying] => ()

  val tdx2: Serializable & Type[Int] = ???
  Type.of[Int] match // ok
    case '[tdx2.Underlying] => ()

  val tdx3: Type[Int] & Serializable & Cloneable = ???
  Type.of[Int] match // ok
    case '[tdx3.Underlying] => ()

  val tdx4: Type[Int] & Type[String] = ??? // becomes TypeTree(Int & String) and Int & String <: Int
  Type.of[Int] match // ok
    case '[tdx4.Underlying] => ()

  type AliasedType = Type[String] & Serializable
  val tdx5: AliasedType = ???
  Type.of[Int] match // warn
    case '[tdx5.Underlying] => ()
  
  type AliasedType2 = Type[Int] & Serializable
  val tdx6: AliasedType2 = ???
  Type.of[Int] match // ok
    case '[tdx6.Underlying] => ()

  lazy val tlazy: Type[Int] & Serializable = ???
  Type.of[Int] match // ok
    case '[tlazy.Underlying] => ()

  val tdxHkt: Type[List] & Serializable = ???
  Type.of[List] match // warn
    case '[tdxHkt.Underlying] => ()

  val typeReprTpe = quotes.reflect.TypeRepr.of[Any] // impossible to know if TypeRepr is a hkt or not
  typeReprTpe.asType match // warn
    case '[tpe] => ()

