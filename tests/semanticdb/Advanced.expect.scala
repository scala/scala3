package advanced

import scala.language/*=>>scalaShadowing.language.*/.higherKinds/*=>>scalaShadowing.language.higherKinds.*/
import scala.language/*=>>scalaShadowing.language.*/.reflectiveCalls/*=>>scalaShadowing.language.reflectiveCalls.*/

import scala.reflect/*=>>scala.reflect.*/.Selectable/*=>>scala.reflect.Selectable.*/.reflectiveSelectable/*=>>scala.reflect.Selectable.reflectiveSelectable().*/

class /*=>>java.lang.Object#`<init>`().*/C/*<<=advanced.C#*/[T] {
/*<<=advanced.C#`<init>`().*//*<<=advanced.C#`<init>`().(T)*//*<<=advanced.C#(T)*/  def t/*<<=advanced.C#t().*/: T/*=>>advanced.C#(T)*/ = ???/*=>>scala.Predef.`???`().*/
}

class /*=>>java.lang.Object#`<init>`().*/Structural/*<<=advanced.Structural#*/ {
  def s1/*<<=advanced.Structural#s1().*/: { val x/*<<=local0*/: Int/*=>>scala.Int#*/ } = ???/*=>>scala.Predef.`???`().*/
  def s2/*<<=advanced.Structural#s2().*/: { val x/*<<=local1*/: Int/*=>>scala.Int#*/ } = /*=>>java.lang.Object#`<init>`().*/new {/*<<=local2*/ val x/*<<=local3*/: Int/*=>>scala.Int#*/ = ???/*=>>scala.Predef.`???`().*/ }/*=>>local4*//*=>>local2*/
  def s3/*<<=advanced.Structural#s3().*/: { def m/*<<=local5*/(x/*<<=local6*/: Int/*=>>scala.Int#*/): Int/*=>>scala.Int#*/ } = /*=>>java.lang.Object#`<init>`().*/new {/*<<=local7*/ def m/*<<=local8*/(x/*<<=local9*/: Int/*=>>scala.Int#*/): Int/*=>>scala.Int#*/ = ???/*=>>scala.Predef.`???`().*/ }/*=>>local10*//*=>>local7*/
}

class /*=>>java.lang.Object#`<init>`().*/Wildcards/*<<=advanced.Wildcards#*/ {
  def e1/*<<=advanced.Wildcards#e1().*/: List/*=>>scala.package.List#*/[_] = ???/*=>>scala.Predef.`???`().*/
}

object /*=>>java.lang.Object#`<init>`().*/Test/*<<=advanced.Test.*/ {
  /*=>>scala.package.Serializable#*//*=>>scala.*//*=>>_root_*//*=>>advanced.Test.*/val s/*<<=advanced.Test.s.*/ = new Structural/*=>>advanced.Structural#*//*=>>advanced.Structural#`<init>`().*/
  val s1/*<<=advanced.Test.s1.*/ = s/*=>>advanced.Test.s.*/.s1/*=>>advanced.Structural#s1().*/
  val s1x/*<<=advanced.Test.s1x.*/ = /*=>>scala.reflect.Selectable.reflectiveSelectable().*/s/*=>>advanced.Test.s.*/.s1/*=>>advanced.Structural#s1().*//*=>>scala.Selectable#selectDynamic().*/.x
  val s2/*<<=advanced.Test.s2.*/ = s/*=>>advanced.Test.s.*/.s2/*=>>advanced.Structural#s2().*/
  val s2x/*<<=advanced.Test.s2x.*/ = /*=>>scala.reflect.Selectable.reflectiveSelectable().*/s/*=>>advanced.Test.s.*/.s2/*=>>advanced.Structural#s2().*//*=>>scala.Selectable#selectDynamic().*/.x
  val s3/*<<=advanced.Test.s3.*/ = s/*=>>advanced.Test.s.*/.s3/*=>>advanced.Structural#s3().*/
  val s3x/*<<=advanced.Test.s3x.*/ = /*=>>scala.reflect.Selectable.reflectiveSelectable().*/s/*=>>advanced.Test.s.*/.s3/*=>>advanced.Structural#s3().*//*=>>scala.Selectable#applyDynamic().*/.m/*=>>scala.reflect.ClassTag.apply().*//*=>>scala.reflect.ClassTag.*//*=>>java.lang.Integer.TYPE.*//*=>>java.lang.Integer.*/(???/*=>>scala.Predef.`???`().*/)

  val e/*<<=advanced.Test.e.*/ = new Wildcards/*=>>advanced.Wildcards#*//*=>>advanced.Wildcards#`<init>`().*/
  val e1/*<<=advanced.Test.e1.*/ = e/*=>>advanced.Test.e.*/.e1/*=>>advanced.Wildcards#e1().*/
  val e1x/*<<=advanced.Test.e1x.*/ = e/*=>>advanced.Test.e.*/.e1/*=>>advanced.Wildcards#e1().*/.head/*=>>scala.collection.IterableOps#head().*/

  {
    (???/*=>>scala.Predef.`???`().*/ : Any/*=>>scala.Any#*/) match {
      case e3/*<<=local11*/: List/*=>>scala.package.List#*/[_/*<<=local12*/] =>
        val e3x/*<<=local13*/ = e3/*=>>local11*/.head/*=>>scala.collection.IterableOps#head().*/
        ()
    }
  }
}
