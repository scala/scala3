package example

import scala.language/*->scala::language.*/.implicitConversions/*->scala::language.implicitConversions.*/

class Synthetic/*<-example::Synthetic#*/ {
  List/*->scala::package.List.*//*->scala::collection::IterableFactory#apply().*/(1).map/*->scala::collection::immutable::List#map().*/(_ +/*->scala::Int#`+`(+4).*/ 2)
  /*->scala::Predef.intArrayOps().*/Array/*->scala::Array.*/.empty/*->scala::Array.empty().*/[Int/*->scala::Int#*/]/*->scala::reflect::ClassTag.apply().*/.headOption/*->scala::collection::ArrayOps#headOption().*/
  /*->scala::Predef.augmentString().*/"fooo".stripPrefix/*->scala::collection::StringOps#stripPrefix().*/("o")

  // See https://github.com/scalameta/scalameta/issues/977
  val Name/*<-example::Synthetic#Name.*/ = /*->scala::Predef.augmentString().*/"name:(.*)".r/*->scala::collection::StringOps#r().*/
  val x/*<-example::Synthetic#x.*/ #::/*->scala::package.`#::`.*//*->scala::package.`#::`.unapply().*/ xs/*<-example::Synthetic#xs.*/ = LazyList/*->scala::package.LazyList.*//*->scala::collection::IterableFactory#apply().*/(1, 2)
  val Name/*->example::Synthetic#Name.*//*->scala::util::matching::Regex#unapplySeq().*/(name/*<-example::Synthetic#name.*/) = "name:foo"
  1 #:: /*->scala::collection::immutable::LazyList.toDeferrer().*/2 #:: /*->scala::collection::immutable::LazyList.toDeferrer().*/LazyList/*->scala::package.LazyList.*/.empty/*->scala::collection::immutable::LazyList.empty().*//*->scala::collection::immutable::LazyList.Deferrer#`#::`().*/

  val a1/*<-example::Synthetic#a1.*/ #::/*->scala::package.`#::`.*//*->scala::package.`#::`.unapply().*/ a2/*<-example::Synthetic#a2.*/ #::/*->scala::package.`#::`.*//*->scala::package.`#::`.unapply().*/ as/*<-example::Synthetic#as.*/ = LazyList/*->scala::package.LazyList.*//*->scala::collection::IterableFactory#apply().*/(1, 2)

  val lst/*<-example::Synthetic#lst.*/ = 1 #:: /*->scala::collection::immutable::LazyList.toDeferrer().*/2 #:: /*->scala::collection::immutable::LazyList.toDeferrer().*/LazyList/*->scala::package.LazyList.*/.empty/*->scala::collection::immutable::LazyList.empty().*//*->scala::collection::immutable::LazyList.Deferrer#`#::`().*/

  for (x/*<-local0*/ <- /*->scala::LowPriorityImplicits#intWrapper().*/1 to/*->scala::runtime::RichInt#to().*/ 10/*->scala::collection::immutable::Range#foreach().*/; y/*<-local1*/ <- /*->scala::LowPriorityImplicits#intWrapper().*/0 until/*->scala::runtime::RichInt#until().*/ 10/*->scala::collection::immutable::Range#foreach().*/) println/*->scala::Predef.println(+1).*/(/*->scala::Predef.ArrowAssoc().*/x/*->local0*/ ->/*->scala::Predef.ArrowAssoc#`->`().*/ x/*->local0*/)
  for (i/*<-local2*/ <- /*->scala::LowPriorityImplicits#intWrapper().*/1 to/*->scala::runtime::RichInt#to().*/ 10/*->scala::collection::StrictOptimizedIterableOps#flatMap().*/; j/*<-local3*/ <- /*->scala::LowPriorityImplicits#intWrapper().*/0 until/*->scala::runtime::RichInt#until().*/ 10/*->scala::collection::immutable::Range#map().*/) yield (/*->scala::Tuple2.apply().*/i/*->local2*/, j/*->local3*/)
  for (i/*<-local4*/ <- /*->scala::LowPriorityImplicits#intWrapper().*/1 to/*->scala::runtime::RichInt#to().*/ 10/*->scala::collection::StrictOptimizedIterableOps#flatMap().*/; j/*<-local5*/ <- /*->scala::LowPriorityImplicits#intWrapper().*/0 until/*->scala::runtime::RichInt#until().*/ 10/*->scala::collection::IterableOps#withFilter().*/ if i/*->local4*/ %/*->scala::Int#`%`(+3).*/ 2 ==/*->scala::Int#`==`(+3).*/ 0/*->scala::collection::WithFilter#map().*/) yield (/*->scala::Tuple2.apply().*/i/*->local4*/, j/*->local5*/)

  object s/*<-example::Synthetic#s.*/ {
    def apply/*<-example::Synthetic#s.apply().*/() = 2
    s/*->example::Synthetic#s.apply().*/()
    s.apply/*->example::Synthetic#s.apply().*/()
    case class Bar/*<-example::Synthetic#s.Bar#*/()
    Bar/*->example::Synthetic#s.Bar.*//*->example::Synthetic#s.Bar.apply().*/()
    null.asInstanceOf/*->scala::Any#asInstanceOf().*/[Int/*->scala::Int#*/ => Int/*->scala::Int#*/]/*->scala::Function1#apply().*/(2)
  }

  class J/*<-example::Synthetic#J#*/[T/*<-example::Synthetic#J#[T]*/: Manifest/*->scala::Predef.Manifest#*//*->example::Synthetic#J#[T]*/] { val arr/*<-example::Synthetic#J#arr.*/ = Array/*->scala::Array.*/.empty/*->scala::Array.empty().*/[T/*->example::Synthetic#J#[T]*/]/*->example::Synthetic#J#evidence$1.*/ }

  class F/*<-example::Synthetic#F#*/
  implicit val ordering/*<-example::Synthetic#ordering.*/: Ordering/*->scala::package.Ordering#*/[F/*->example::Synthetic#F#*/] = ???/*->scala::Predef.`???`().*/
  val f/*<-example::Synthetic#f.*/: Ordered/*->scala::package.Ordered#*/[F/*->example::Synthetic#F#*/] = /*->scala::math::Ordered.orderingToOrdered().*/new F/*->example::Synthetic#F#*//*->example::Synthetic#ordering.*/

  import scala.concurrent.ExecutionContext/*->scala::concurrent::ExecutionContext.*/.Implicits/*->scala::concurrent::ExecutionContext.Implicits.*/.global/*->scala::concurrent::ExecutionContext.Implicits.global().*/
  for {
    a/*<-local6*/ <- scala.concurrent.Future/*->scala::concurrent::Future.*/.successful/*->scala::concurrent::Future.successful().*/(1)/*->scala::concurrent::Future#foreach().*/
    b/*<-local7*/ <- scala.concurrent.Future/*->scala::concurrent::Future.*/.successful/*->scala::concurrent::Future.successful().*/(2)/*->scala::concurrent::Future#foreach().*/
  } println/*->scala::Predef.println(+1).*/(a/*->local6*/)/*->scala::concurrent::ExecutionContext.Implicits.global().*/
  for {
    a/*<-local8*/ <- scala.concurrent.Future/*->scala::concurrent::Future.*/.successful/*->scala::concurrent::Future.successful().*/(1)/*->scala::concurrent::Future#flatMap().*/
    b/*<-local9*/ <- scala.concurrent.Future/*->scala::concurrent::Future.*/.successful/*->scala::concurrent::Future.successful().*/(2)/*->scala::concurrent::Future#withFilter().*/
    if a/*->local8*/ </*->scala::Int#`<`(+3).*/ b/*->local9*//*->scala::concurrent::ExecutionContext.Implicits.global().*//*->scala::concurrent::Future#map().*/
  } yield a/*->local8*//*->scala::concurrent::ExecutionContext.Implicits.global().*/

}
