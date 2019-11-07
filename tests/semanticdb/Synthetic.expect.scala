package example

class Synthetic/*<<=example.Synthetic#*/ {
  List/*=>>scala.package.List.*//*=>>scala.collection.IterableFactory#apply().*/(1).map/*=>>scala.collection.immutable.List#map().*/(_ +/*=>>scala.Int#`+`(+4).*/ 2)
  /*=>>scala.Predef.intArrayOps().*/Array/*=>>scala.Array.*/.empty/*=>>scala.Array.empty().*/[Int/*=>>scala.Int#*/]/*=>>scala.reflect.ClassTag.apply().*//*=>>java.lang.Integer#TYPE.*/.headOption/*=>>scala.collection.ArrayOps#headOption().*/
  /*=>>scala.Predef.augmentString().*/"fooo".stripPrefix/*=>>scala.collection.StringOps#stripPrefix().*/("o")

  // See https://github.com/scalameta/scalameta/issues/977
  val Name/*<<=example.Synthetic#Name.*/ = /*=>>scala.Predef.augmentString().*/"name:(.*)".r/*=>>scala.collection.StringOps#r().*/
  /*=>>scala.Tuple2#_1.*//*=>>scala.Tuple2#_2.*/val x/*<<=example.Synthetic#x.*/ #:: xs/*<<=example.Synthetic#xs.*/ = Stream(1, 2)
  val Name/*=>>example.Synthetic#Name.*//*=>>scala.util.matching.Regex#unapplySeq().*/(name/*<<=example.Synthetic#name.*//*<<=local0*/)/*=>>local0*/ = "name:foo"
  1 #:: /*=>>scala.collection.immutable.Stream.toDeferrer().*/2 #:: /*=>>scala.collection.immutable.Stream.toDeferrer().*/Stream/*=>>scala.package.Stream.*/.empty/*=>>scala.collection.immutable.Stream.empty().*//*=>>scala.collection.immutable.Stream.Deferrer#`#::`().*/

  val lst/*<<=example.Synthetic#lst.*/ = 1 #:: /*=>>scala.collection.immutable.Stream.toDeferrer().*/2 #:: /*=>>scala.collection.immutable.Stream.toDeferrer().*/Stream/*=>>scala.package.Stream.*/.empty/*=>>scala.collection.immutable.Stream.empty().*//*=>>scala.collection.immutable.Stream.Deferrer#`#::`().*/

  for (x <- /*=>>scala.LowPriorityImplicits#intWrapper().*/1 to/*=>>scala.runtime.RichInt#to().*/ 10/*=>>scala.collection.immutable.Range#foreach().*/; y <- /*=>>scala.LowPriorityImplicits#intWrapper().*/0 until/*=>>scala.runtime.RichInt#until().*/ 10/*=>>scala.collection.immutable.Range#foreach().*/) println/*=>>scala.Predef.println(+1).*/(x/*=>>local1*/ ->/*=>>scala.Predef.ArrowAssoc#`->`().*/ x/*=>>local1*/)
  for (i <- /*=>>scala.LowPriorityImplicits#intWrapper().*/1 to/*=>>scala.runtime.RichInt#to().*/ 10/*=>>scala.collection.StrictOptimizedIterableOps#flatMap().*/; j <- /*=>>scala.LowPriorityImplicits#intWrapper().*/0 until/*=>>scala.runtime.RichInt#until().*/ 10/*=>>scala.collection.immutable.Range#map().*/) yield (i/*=>>local2*/, j/*=>>local3*/)
  for (i <- /*=>>scala.LowPriorityImplicits#intWrapper().*/1 to/*=>>scala.runtime.RichInt#to().*/ 10/*=>>scala.collection.StrictOptimizedIterableOps#flatMap().*/; j <- /*=>>scala.LowPriorityImplicits#intWrapper().*/0 until/*=>>scala.runtime.RichInt#until().*/ 10/*=>>scala.collection.IterableOps#withFilter().*/ if i/*=>>local4*/ %/*=>>scala.Int#`%`(+3).*/ 2 ==/*=>>scala.Int#`==`(+3).*/ 0/*=>>scala.collection.WithFilter#map().*/) yield (i/*=>>local4*/, j/*=>>local5*/)

  object s/*<<=example.Synthetic#s.*/ {
    def apply/*<<=example.Synthetic#s.apply().*/() = 2
    s/*=>>example.Synthetic#s.apply().*/()
    s.apply/*=>>example.Synthetic#s.apply().*/()
    case class Bar/*<<=example.Synthetic#s.Bar#*/()
    Bar/*=>>example.Synthetic#s.Bar.*/()
    null.asInstanceOf/*=>>scala.Any#asInstanceOf().*/[Int/*=>>scala.Int#*/ => Int/*=>>scala.Int#*/]/*=>>scala.Function1#apply().*/(2)
  }

  class J/*<<=example.Synthetic#J#*/[T/*<<=example.Synthetic#J#[T]*//*<<=example.Synthetic#J#evidence$2.*/: Manifest/*=>>scala.Predef.Manifest#*//*=>>example.Synthetic#J#`<init>`().[T]*/] { val arr/*<<=example.Synthetic#J#arr.*/ = Array/*=>>scala.Array.*/.empty/*=>>scala.Array.empty().*/[T/*=>>example.Synthetic#J#[T]*/]/*=>>example.Synthetic#J#evidence$2.*/ }

  class F/*<<=example.Synthetic#F#*/
  implicit val ordering/*<<=example.Synthetic#ordering.*/: Ordering/*=>>scala.package.Ordering#*/[F/*=>>example.Synthetic#F#*/] = ???/*=>>scala.Predef.`???`().*/
  val f/*<<=example.Synthetic#f.*/: Ordered/*=>>scala.package.Ordered#*/[F/*=>>example.Synthetic#F#*/] = /*=>>scala.math.Ordered.orderingToOrdered().*/new F/*=>>example.Synthetic#F#*//*=>>example.Synthetic#F#`<init>`().*//*=>>example.Synthetic#ordering.*/

  import scala.concurrent.ExecutionContext/*=>>scala.concurrent.ExecutionContext.*/.Implicits/*=>>scala.concurrent.ExecutionContext.Implicits.*/.global/*=>>scala.concurrent.ExecutionContext.Implicits.global().*/
  for {
    a <- scala.concurrent.Future/*=>>scala.concurrent.Future.*/.successful/*=>>scala.concurrent.Future.successful().*/(1)/*=>>scala.concurrent.Future#foreach().*/
    b <- scala.concurrent.Future/*=>>scala.concurrent.Future.*/.successful/*=>>scala.concurrent.Future.successful().*/(2)/*=>>scala.concurrent.Future#foreach().*/
  } println/*=>>scala.Predef.println(+1).*/(a/*=>>local6*/)/*=>>scala.concurrent.ExecutionContext.Implicits.global().*/
  for {
    a <- scala.concurrent.Future/*=>>scala.concurrent.Future.*/.successful/*=>>scala.concurrent.Future.successful().*/(1)/*=>>scala.concurrent.Future#flatMap().*/
    b <- scala.concurrent.Future/*=>>scala.concurrent.Future.*/.successful/*=>>scala.concurrent.Future.successful().*/(2)/*=>>scala.concurrent.Future#withFilter().*/
    if a/*=>>local7*/ </*=>>scala.Int#`<`(+3).*/ b/*=>>local8*//*=>>scala.concurrent.Future#map().*//*=>>scala.concurrent.ExecutionContext.Implicits.global().*/
  } yield a/*=>>local7*//*=>>scala.concurrent.ExecutionContext.Implicits.global().*/

}
