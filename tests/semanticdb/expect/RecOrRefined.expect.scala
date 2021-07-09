package example

def m1/*<-example::RecOrRefined$package.m1().*/(a/*<-example::RecOrRefined$package.m1().(a)*/: Int/*->scala::Int#*/ { val x/*<-local0*/: Int/*->scala::Int#*/ }) = ???/*->scala::Predef.`???`().*/
def m2/*<-example::RecOrRefined$package.m2().*/(x/*<-example::RecOrRefined$package.m2().(x)*/: { val x/*<-local1*/: Int/*->scala::Int#*/; def y/*<-local2*/: Int/*->scala::Int#*/ }) = ???/*->scala::Predef.`???`().*/
def m3/*<-example::RecOrRefined$package.m3().*/(x/*<-example::RecOrRefined$package.m3().(x)*/: { val x/*<-local3*/: Int/*->scala::Int#*/; def y/*<-local4*/: Int/*->scala::Int#*/; type z/*<-local5*/ }) = ???/*->scala::Predef.`???`().*/
trait PolyHolder/*<-example::PolyHolder#*/ {
  def foo/*<-example::PolyHolder#foo().*/[T/*<-example::PolyHolder#foo().[T]*/](t/*<-example::PolyHolder#foo().(t)*/: T/*->example::PolyHolder#foo().[T]*/): Any/*->scala::Any#*/
}

def m4/*<-example::RecOrRefined$package.m4().*/(x/*<-example::RecOrRefined$package.m4().(x)*/: PolyHolder/*->example::PolyHolder#*/ { def foo/*<-local8*/[T/*<-local6*/](t/*<-local7*/: T/*->local6*/): T/*->local6*/ }) = ???/*->scala::Predef.`???`().*/
def m5/*<-example::RecOrRefined$package.m5().*/[Z/*<-example::RecOrRefined$package.m5().[Z]*/](x/*<-example::RecOrRefined$package.m5().(x)*/: Int/*->scala::Int#*/): PolyHolder/*->example::PolyHolder#*/ { def foo/*<-local11*/[T/*<-local9*/](t/*<-local10*/: T/*->local9*/): T/*->local9*/ } = ???/*->scala::Predef.`???`().*/

type m6/*<-example::RecOrRefined$package.m6#*/ = [X/*<-example::RecOrRefined$package.m6#[X]*/] =>> PolyHolder/*->example::PolyHolder#*/ { def foo/*<-local14*/[T/*<-local12*/](t/*<-local13*/: T/*->local12*/): T/*->local12*/ }

class Record/*<-example::Record#*/(elems/*<-example::Record#elems.*/: (String/*->scala::Predef.String#*/, Any/*->scala::Any#*/)*) extends Selectable/*->scala::Selectable#*/:
  private val fields/*<-example::Record#fields.*/ = elems/*->example::Record#elems.*/.toMap/*->scala::collection::IterableOnceOps#toMap().*//*->scala::`<:<`.refl().*/
  def selectDynamic/*<-example::Record#selectDynamic().*/(name/*<-example::Record#selectDynamic().(name)*/: String/*->scala::Predef.String#*/): Any/*->scala::Any#*/ = fields/*->example::Record#fields.*//*->scala::collection::MapOps#apply().*/(name/*->example::Record#selectDynamic().(name)*/)

type Person/*<-example::RecOrRefined$package.Person#*/ = Record/*->example::Record#*/ {
  val name/*<-local15*/: String/*->scala::Predef.String#*/
  val age/*<-local16*/: Int/*->scala::Int#*/
}

// RecType
class C/*<-example::C#*/ { type T1/*<-example::C#T1#*/; type T2/*<-example::C#T2#*/ }
type C2/*<-example::RecOrRefined$package.C2#*/ = C/*->example::C#*/ { type T1/*<-local17*/; type T2/*<-local18*/ = T1/*->local17*/ }
