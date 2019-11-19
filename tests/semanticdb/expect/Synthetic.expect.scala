package example

import scala.language/*->scalaShadowing::language.*/.implicitConversions/*->scalaShadowing::language.implicitConversions.*/

class Synthetic/*<-example::Synthetic#*/ {
  List/*->scala::package.List.*//*->scala::collection::IterableFactory#apply().*/(1).map/*->scala::collection::immutable::List#map().*/(_ +/*->scala::Int#`+`(+4).*/ 2)
  /*->scala::Predef.intArrayOps().*/Array/*->scala::Array.*/.empty/*->scala::Array.empty().*/[Int/*->scala::Int#*/]/*->scala::reflect::ClassTag.apply().*//*->java::lang::Integer#TYPE.*/.headOption/*->scala::collection::ArrayOps#headOption().*/
  /*->scala::Predef.augmentString().*/"fooo".stripPrefix/*->scala::collection::StringOps#stripPrefix().*/("o")

  // See https://github.com/scalameta/scalameta/issues/977
  val Name/*<-example::Synthetic#Name.*/ = /*->scala::Predef.augmentString().*/"name:(.*)".r/*->scala::collection::StringOps#r().*/
  /*->scala::Tuple2#_1.*//*->scala::Tuple2#_2.*/val x/*<-local0*//*<-example::Synthetic#x.*/ #::/*->scala::package.`#::`.*//*->scala::package.`#::`.unapply().*/ xs/*<-local1*//*<-example::Synthetic#xs.*//*->local0*//*->local1*/ = LazyList/*->scala::package.LazyList.*//*->scala::collection::IterableFactory#apply().*/(1, 2)
  val Name/*->example::Synthetic#Name.*//*->scala::util::matching::Regex#unapplySeq().*/(name/*<-example::Synthetic#name.*//*<-local2*/)/*->local2*/ = "name:foo"
  1 #:: /*->scala::collection::immutable::LazyList.toDeferrer().*/2 #:: /*->scala::collection::immutable::LazyList.toDeferrer().*/LazyList/*->scala::package.LazyList.*/.empty/*->scala::collection::immutable::LazyList.empty().*//*->scala::collection::immutable::LazyList.Deferrer#`#::`().*/

  val lst/*<-example::Synthetic#lst.*/ = 1 #:: /*->scala::collection::immutable::LazyList.toDeferrer().*/2 #:: /*->scala::collection::immutable::LazyList.toDeferrer().*/LazyList/*->scala::package.LazyList.*/.empty/*->scala::collection::immutable::LazyList.empty().*//*->scala::collection::immutable::LazyList.Deferrer#`#::`().*/

  for (x/*<-local3*/ <- /*->scala::LowPriorityImplicits#intWrapper().*/1 to/*->scala::runtime::RichInt#to().*/ 10/*->scala::collection::immutable::Range#foreach().*/; y/*<-local4*/ <- /*->scala::LowPriorityImplicits#intWrapper().*/0 until/*->scala::runtime::RichInt#until().*/ 10/*->scala::collection::immutable::Range#foreach().*/) println/*->scala::Predef.println(+1).*/(x/*->local3*/ ->/*->scala::Predef.ArrowAssoc#`->`().*/ x/*->local3*/)
  for (i/*<-local5*/ <- /*->scala::LowPriorityImplicits#intWrapper().*/1 to/*->scala::runtime::RichInt#to().*/ 10/*->scala::collection::StrictOptimizedIterableOps#flatMap().*/; j/*<-local6*/ <- /*->scala::LowPriorityImplicits#intWrapper().*/0 until/*->scala::runtime::RichInt#until().*/ 10/*->scala::collection::immutable::Range#map().*/) yield (i/*->local5*/, j/*->local6*/)
  for (i/*<-local7*/ <- /*->scala::LowPriorityImplicits#intWrapper().*/1 to/*->scala::runtime::RichInt#to().*/ 10/*->scala::collection::StrictOptimizedIterableOps#flatMap().*/; j/*<-local8*/ <- /*->scala::LowPriorityImplicits#intWrapper().*/0 until/*->scala::runtime::RichInt#until().*/ 10/*->scala::collection::IterableOps#withFilter().*/ if i/*->local7*/ %/*->scala::Int#`%`(+3).*/ 2 ==/*->scala::Int#`==`(+3).*/ 0/*->scala::collection::WithFilter#map().*/) yield (i/*->local7*/, j/*->local8*/)

  object s/*<-example::Synthetic#s.*/ {
    def apply/*<-example::Synthetic#s.apply().*/() = 2
    s/*->example::Synthetic#s.apply().*/()
    s.apply/*->example::Synthetic#s.apply().*/()
    case class Bar/*<-example::Synthetic#s.Bar#*/()
    Bar/*->example::Synthetic#s.Bar.*/()
    null.asInstanceOf/*->scala::Any#asInstanceOf().*/[Int/*->scala::Int#*/ => Int/*->scala::Int#*/]/*->scala::Function1#apply().*/(2)
  }

  class J/*<-example::Synthetic#J#*/[T/*<-example::Synthetic#J#[T]*/: Manifest/*->scala::Predef.Manifest#*//*->example::Synthetic#J#[T]*/] { val arr/*<-example::Synthetic#J#arr.*/ = Array/*->scala::Array.*/.empty/*->scala::Array.empty().*/[T/*->example::Synthetic#J#[T]*/] }

  class F/*<-example::Synthetic#F#*/
  implicit val ordering/*<-example::Synthetic#ordering.*/: Ordering/*->scala::package.Ordering#*/[F/*->example::Synthetic#F#*/] = ???/*->scala::Predef.`???`().*/
  val f/*<-example::Synthetic#f.*/: Ordered/*->scala::package.Ordered#*/[F/*->example::Synthetic#F#*/] = /*->scala::math::Ordered.orderingToOrdered().*/new F/*->example::Synthetic#F#*//*->example::Synthetic#ordering.*/

  import scala.concurrent.ExecutionContext/*->scala::concurrent::ExecutionContext.*/.Implicits/*->scala::concurrent::ExecutionContext.Implicits.*/.global/*->scala::concurrent::ExecutionContext.Implicits.global().*/
  for {
    a/*<-local9*/ <- scala.concurrent.Future/*->scala::concurrent::Future.*/.successful/*->scala::concurrent::Future.successful().*/(1)/*->scala::concurrent::Future#foreach().*/
    b/*<-local10*/ <- scala.concurrent.Future/*->scala::concurrent::Future.*/.successful/*->scala::concurrent::Future.successful().*/(2)/*->scala::concurrent::Future#foreach().*/
  } println/*->scala::Predef.println(+1).*/(a/*->local9*/)/*->scala::concurrent::ExecutionContext.Implicits.global().*/
  for {
    a/*<-local11*/ <- scala.concurrent.Future/*->scala::concurrent::Future.*/.successful/*->scala::concurrent::Future.successful().*/(1)/*->scala::concurrent::Future#flatMap().*/
    b/*<-local12*/ <- scala.concurrent.Future/*->scala::concurrent::Future.*/.successful/*->scala::concurrent::Future.successful().*/(2)/*->scala::concurrent::Future#withFilter().*/
    if a/*->local11*/ </*->scala::Int#`<`(+3).*/ b/*->local12*//*->scala::concurrent::Future#map().*//*->scala::concurrent::ExecutionContext.Implicits.global().*/
  } yield a/*->local11*//*->scala::concurrent::ExecutionContext.Implicits.global().*/

}
