package types

import scala.language/*->scala::language.*/.existentials/*->scala::language.existentials.*/
import scala.language/*->scala::language.*/.higherKinds/*->scala::language.higherKinds.*/

class ann/*<-types::ann#*/[T/*<-types::ann#[T]*/](x/*<-types::ann#x.*/: T/*->types::ann#[T]*/) extends scala.annotation.StaticAnnotation/*->scala::annotation::StaticAnnotation#*/
class ann1/*<-types::ann1#*/ extends scala.annotation.StaticAnnotation/*->scala::annotation::StaticAnnotation#*/
class ann2/*<-types::ann2#*/ extends scala.annotation.StaticAnnotation/*->scala::annotation::StaticAnnotation#*/

class B/*<-types::B#*/

class C/*<-types::C#*/

class P/*<-types::P#*/ {
  class C/*<-types::P#C#*/
  class X/*<-types::P#X#*/
  val x/*<-types::P#x.*/ = new X/*->types::P#X#*/
}

class T/*<-types::T#*/ {
  class C/*<-types::T#C#*/
  class X/*<-types::T#X#*/
  val x/*<-types::T#x.*/ = new X/*->types::T#X#*/
}

case class Foo/*<-types::Foo#*/(s/*<-types::Foo#s.*/: "abc")

object Foo/*<-types::Foo.*/ {
  val x/*<-types::Foo.x.*/: "abc" @deprecated/*->scala::deprecated#*/ = "abc"
  val y/*<-types::Foo.y.*/: "abc" = x/*->types::Foo.x.*/
}

object Test/*<-types::Test.*/ {
  class M/*<-types::Test.M#*/ {
    def m/*<-types::Test.M#m().*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
  }

  trait N/*<-types::Test.N#*/ {
    def n/*<-types::Test.N#n().*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
  }

  class C/*<-types::Test.C#*/ extends M/*->types::Test.M#*/ {
    val p/*<-types::Test.C#p.*/ = new P/*->types::P#*/
    val x/*<-types::Test.C#x.*/ = p/*->types::Test.C#p.*/.x/*->types::P#x.*/

    val typeRef1/*<-types::Test.C#typeRef1.*/: C/*->types::Test.C#*/ = ???/*->scala::Predef.`???`().*/
    val typeRef2/*<-types::Test.C#typeRef2.*/: p/*->types::Test.C#p.*/.C/*->types::P#C#*/ = ???/*->scala::Predef.`???`().*/
    val typeRef3/*<-types::Test.C#typeRef3.*/: T/*->types::T#*/#C/*->types::T#C#*/ = ???/*->scala::Predef.`???`().*/
    val typeRef4/*<-types::Test.C#typeRef4.*/: List/*->scala::package.List#*/[Int/*->scala::Int#*/] = ???/*->scala::Predef.`???`().*/

    val singleType1/*<-types::Test.C#singleType1.*/: x/*->types::Test.C#x.*/.type = ???/*->scala::Predef.`???`().*/
    val singleType2/*<-types::Test.C#singleType2.*/: p/*->types::Test.C#p.*/.x/*->types::P#x.*/.type = ???/*->scala::Predef.`???`().*/
    val Either/*<-types::Test.C#Either.*/ = scala.util.Either/*->scala::util::Either.*/

    val thisType1/*<-types::Test.C#thisType1.*/: this.type = ???/*->scala::Predef.`???`().*/
    val thisType2/*<-types::Test.C#thisType2.*/: C.this.type = ???/*->scala::Predef.`???`().*/

    val superType1/*<-types::Test.C#superType1.*/ = super.m/*->types::Test.M#m().*/
    val superType2/*<-types::Test.C#superType2.*/ = super[M].m/*->types::Test.M#m().*/
    val superType3/*<-types::Test.C#superType3.*/ = C.super[M].m/*->types::Test.M#m().*/

    val compoundType1/*<-types::Test.C#compoundType1.*/: { def k/*<-local0*/: Int/*->scala::Int#*/ } = ???/*->scala::Predef.`???`().*/
    val compoundType2/*<-types::Test.C#compoundType2.*/: M/*->types::Test.M#*/ with N/*->types::Test.N#*/ = ???/*->scala::Predef.`???`().*/
    val compoundType3/*<-types::Test.C#compoundType3.*/: M/*->types::Test.M#*/ with N/*->types::Test.N#*/ { def k/*<-local2*/: Int/*->scala::Int#*/ } = ???/*->scala::Predef.`???`().*/
    val compoundType4/*<-types::Test.C#compoundType4.*/ = new { def k/*<-local4*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/ }
    val compoundType5/*<-types::Test.C#compoundType5.*/ = new M/*->types::Test.M#*/ with N/*->types::Test.N#*/
    val compoundType6/*<-types::Test.C#compoundType6.*/ = new M/*->types::Test.M#*/ with N/*->types::Test.N#*/ { def k/*<-local9*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/ }

    val annType1/*<-types::Test.C#annType1.*/: T/*->types::T#*/ @ann(42) = ???/*->scala::Predef.`???`().*/
    val annType2/*<-types::Test.C#annType2.*/: T/*->types::T#*/ @ann1/*->types::ann1#*/ @ann2/*->types::ann2#*/ = ???/*->scala::Predef.`???`().*/

    val existentialType2/*<-types::Test.C#existentialType2.*/: List/*->scala::package.List#*/[_] = ???/*->scala::Predef.`???`().*/
    val existentialType3/*<-types::Test.C#existentialType3.*/ = Class/*->java::lang::Class#*/.forName/*->java::lang::Class#forName().*/("foo.Bar")
    val existentialType4/*<-types::Test.C#existentialType4.*/ = Class/*->java::lang::Class#*/.forName/*->java::lang::Class#forName().*/("foo.Bar")

    def typeLambda1/*<-types::Test.C#typeLambda1().*/[M/*<-types::Test.C#typeLambda1().[M]*/[_]] = ???/*->scala::Predef.`???`().*/
    typeLambda1/*->types::Test.C#typeLambda1().*/[({ type L/*<-local13*/[T/*<-local12*/] = List/*->scala::package.List#*/[T/*->local12*/] })#L]

    object ClassInfoType1/*<-types::Test.C#ClassInfoType1.*/
    class ClassInfoType2/*<-types::Test.C#ClassInfoType2#*/ extends B/*->types::B#*/ { def x/*<-types::Test.C#ClassInfoType2#x().*/ = 42 }
    trait ClassInfoType3/*<-types::Test.C#ClassInfoType3#*/[T/*<-types::Test.C#ClassInfoType3#[T]*/]

    object MethodType/*<-types::Test.C#MethodType.*/ {
      def x1/*<-types::Test.C#MethodType.x1().*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
      def x2/*<-types::Test.C#MethodType.x2().*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
      def m3/*<-types::Test.C#MethodType.m3().*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
      def m4/*<-types::Test.C#MethodType.m4().*/(): Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
      def m5/*<-types::Test.C#MethodType.m5().*/(x/*<-types::Test.C#MethodType.m5().(x)*/: Int/*->scala::Int#*/): Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
      def m6/*<-types::Test.C#MethodType.m6().*/[T/*<-types::Test.C#MethodType.m6().[T]*/](x/*<-types::Test.C#MethodType.m6().(x)*/: T/*->types::Test.C#MethodType.m6().[T]*/): T/*->types::Test.C#MethodType.m6().[T]*/ = ???/*->scala::Predef.`???`().*/
    }

    object ByNameType/*<-types::Test.C#ByNameType.*/ {
      def m1/*<-types::Test.C#ByNameType.m1().*/(x/*<-types::Test.C#ByNameType.m1().(x)*/: => Int/*->scala::Int#*/): Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
    }

    case class RepeatedType/*<-types::Test.C#RepeatedType#*/(s/*<-types::Test.C#RepeatedType#s.*/: String/*->scala::Predef.String#*/*) {
      def m1/*<-types::Test.C#RepeatedType#m1().*/(x/*<-types::Test.C#RepeatedType#m1().(x)*/: Int/*->scala::Int#*/*): Int/*->scala::Int#*/ = s/*->types::Test.C#RepeatedType#s.*/.length/*->scala::collection::SeqOps#length().*/
    }

    object TypeType/*<-types::Test.C#TypeType.*/ {
      type T1/*<-types::Test.C#TypeType.T1#*/
      def m2/*<-types::Test.C#TypeType.m2().*/[T2/*<-types::Test.C#TypeType.m2().[T2]*/ >: C/*->types::Test.C#*/ <: C/*->types::Test.C#*/] = ???/*->scala::Predef.`???`().*/
      def m3/*<-types::Test.C#TypeType.m3().*/[M3/*<-types::Test.C#TypeType.m3().[M3]*/[_]] = ???/*->scala::Predef.`???`().*/
      type T4/*<-types::Test.C#TypeType.T4#*/ = C/*->types::Test.C#*/
      type T5/*<-types::Test.C#TypeType.T5#*/[U/*<-types::Test.C#TypeType.T5#[U]*/] = U/*->types::Test.C#TypeType.T5#[U]*/
    }
  }

  object Literal/*<-types::Test.Literal.*/ {
    final val int/*<-types::Test.Literal.int.*/ = 1
    final val long/*<-types::Test.Literal.long.*/ = 1L
    final val float/*<-types::Test.Literal.float.*/ = 1f
    final val double/*<-types::Test.Literal.double.*/ = 2d
    final val nil/*<-types::Test.Literal.nil.*/ = null
    final val char/*<-types::Test.Literal.char.*/ = 'a'
    final val string/*<-types::Test.Literal.string.*/ = "a"
    final val bool/*<-types::Test.Literal.bool.*/ = true
    final val unit/*<-types::Test.Literal.unit.*/ = ()
    final val javaEnum/*<-types::Test.Literal.javaEnum.*/ = java.nio.file.LinkOption/*->java::nio::file::LinkOption#*/.NOFOLLOW_LINKS/*->java::nio::file::LinkOption#NOFOLLOW_LINKS.*/
    final val clazzOf/*<-types::Test.Literal.clazzOf.*/ = classOf/*->scala::Predef.classOf().*/[Option/*->scala::Option#*/[Int/*->scala::Int#*/]]
  }
}
