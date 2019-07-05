import scala.quoted._
object Test {

  def x given QuoteContext = '{1 + 2}

  def f(x: Int) = x
  def g(x: Int, y: Int) = x * y

  def res given QuoteContext: quoted.Expr[Int] = x match {
    case '{1 + 2} => '{0}
    case '{f($y)} => y
    case '{g($y, $z)} => '{$y * $z}
    case '{ ((a: Int) => 3)($y) } => y
    case '{ 1 + ($y: Int)} => y
    case '{ val a = 1 + ($y: Int); 3 } => y
    case '{ val $y: Int = $z; println(`$y`); 1 } =>
      val a: quoted.matching.Bind[Int] = y
      z
    case '{ (($y: Int) => 1 + `$y` + ($z: Int))(2) } =>
      val a: quoted.matching.Bind[Int] = y
      z
    case '{ def $ff: Int = $z; `$ff` } =>
      val a: quoted.matching.Bind[Int] = ff
      z
    case '{ def $ff(i: Int): Int = $z; 2 } =>
      val a: quoted.matching.Bind[Int => Int] = ff
      z
    case '{ def $ff(i: Int)(j: Int): Int = $z; 2 } =>
      val a: quoted.matching.Bind[Int => Int => Int] = ff
      z
    case '{ def $ff[T](i: T): Int = $z; 2 } =>
      val a: quoted.matching.Bind[[T] =>> T => Int] = ff
      z
    case '{ poly[$t]($x); 4 } => ???
    case '{ poly[${Foo(t)}]($x); 4 } => ???
    case '{ type $X; poly[`$X`]($x); 4 } => ???
    case '{ type $t; poly[${Foo(x: quoted.Type[`$t`])}]($x); 4 } => ???
    case '{ type $T; val x: `$T` = $a; val y: `$T` = x;  1 } => ???
    case '{ type $t <: AnyRef; val x: `$t` = $a; val y: `$t` = x;  1 } => ???
    case _ => '{1}
  }

  def poly[T](x: T): Unit = ()

  object Foo {
    def unapply[T](arg: quoted.Type[T]): Option[quoted.Type[T]] = Some(arg)
  }
}