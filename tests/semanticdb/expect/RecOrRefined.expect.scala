package example

def m1/*<-example::RecOrRefined$package.m1().*/(a/*<-example::RecOrRefined$package.m1().(a)*/: Int/*->scala::Int#*/ { val x/*<-local0*/: Int/*->scala::Int#*/ }) = ???/*->scala::Predef.`???`().*/
def m2/*<-example::RecOrRefined$package.m2().*/(x/*<-example::RecOrRefined$package.m2().(x)*/: { val x/*<-local1*/: Int/*->scala::Int#*/; def y/*<-local2*/: Int/*->scala::Int#*/ }) = ???/*->scala::Predef.`???`().*/
def m3/*<-example::RecOrRefined$package.m3().*/(x/*<-example::RecOrRefined$package.m3().(x)*/: { val x/*<-local3*/: Int/*->scala::Int#*/; def y/*<-local4*/: Int/*->scala::Int#*/; type z/*<-local5*/ }) = ???/*->scala::Predef.`???`().*/

class Record/*<-example::Record#*/(elems/*<-example::Record#elems.*/: (String/*->scala::Predef.String#*/, Any/*->scala::Any#*/)*) extends Selectable/*->scala::Selectable#*/:
  private val fields/*<-example::Record#fields.*/ = elems/*->example::Record#elems.*/.toMap/*->scala::collection::IterableOnceOps#toMap().*//*->scala::`<:<`.refl().*/
  def selectDynamic/*<-example::Record#selectDynamic().*/(name/*<-example::Record#selectDynamic().(name)*/: String/*->scala::Predef.String#*/): Any/*->scala::Any#*/ = fields/*->example::Record#fields.*//*->scala::collection::MapOps#apply().*/(name/*->example::Record#selectDynamic().(name)*/)

type Person/*<-example::RecOrRefined$package.Person#*/ = Record/*->example::Record#*/ {
  val name/*<-local6*/: String/*->scala::Predef.String#*/
  val age/*<-local7*/: Int/*->scala::Int#*/
}

// RecType
class C/*<-example::C#*/ { type T1/*<-example::C#T1#*/; type T2/*<-example::C#T2#*/ }
type C2/*<-example::RecOrRefined$package.C2#*/ = C/*->example::C#*/ { type T1/*<-local8*/; type T2/*<-local9*/ = T1/*->local8*/ }
