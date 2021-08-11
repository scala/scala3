package example

def m1/*<-example::RecOrRefined$package.m1().*/(a/*<-example::RecOrRefined$package.m1().(a)*/: Int/*->scala::Int#*/ { val x/*<-local3*/: Int/*->scala::Int#*/ }) = ???/*->scala::Predef.`???`().*/
def m2/*<-example::RecOrRefined$package.m2().*/(x/*<-example::RecOrRefined$package.m2().(x)*/: { val x/*<-local4*/: Int/*->scala::Int#*/; def y/*<-local5*/: Int/*->scala::Int#*/ }) = ???/*->scala::Predef.`???`().*/
def m3/*<-example::RecOrRefined$package.m3().*/(x/*<-example::RecOrRefined$package.m3().(x)*/: { val x/*<-local6*/: Int/*->scala::Int#*/; def y/*<-local7*/: Int/*->scala::Int#*/; type z/*<-local8*/ }) = ???/*->scala::Predef.`???`().*/
trait PolyHolder/*<-example::PolyHolder#*/ {
  def foo/*<-example::PolyHolder#foo().*/[T/*<-example::PolyHolder#foo().[T]*/](t/*<-example::PolyHolder#foo().(t)*/: T/*->example::PolyHolder#foo().[T]*/): Any/*->scala::Any#*/
}

def m4/*<-example::RecOrRefined$package.m4().*/(x/*<-example::RecOrRefined$package.m4().(x)*/: PolyHolder/*->example::PolyHolder#*/ { def foo/*<-local11*/[T/*<-local9*/](t/*<-local10*/: T/*->local9*/): T/*->local9*/ }) = ???/*->scala::Predef.`???`().*/
def m5/*<-example::RecOrRefined$package.m5().*/[Z/*<-example::RecOrRefined$package.m5().[Z]*/](x/*<-example::RecOrRefined$package.m5().(x)*/: Int/*->scala::Int#*/): PolyHolder/*->example::PolyHolder#*/ { def foo/*<-local14*/[T/*<-local12*/](t/*<-local13*/: T/*->local12*/): T/*->local12*/ } = ???/*->scala::Predef.`???`().*/

type m6/*<-example::RecOrRefined$package.m6#*/ = [X/*<-example::RecOrRefined$package.m6#[X]*/] =>> PolyHolder/*->example::PolyHolder#*/ { def foo/*<-local17*/[T/*<-local15*/](t/*<-local16*/: T/*->local15*/): T/*->local15*/ }

class Record/*<-example::Record#*/(elems/*<-example::Record#elems.*/: (String/*->scala::Predef.String#*/, Any/*->scala::Any#*/)*) extends Selectable/*->scala::Selectable#*/:
  private val fields/*<-example::Record#fields.*/ = elems/*->example::Record#elems.*/.toMap/*->scala::collection::IterableOnceOps#toMap().*/
  def selectDynamic/*<-example::Record#selectDynamic().*/(name/*<-example::Record#selectDynamic().(name)*/: String/*->scala::Predef.String#*/): Any/*->scala::Any#*/ = fields/*->example::Record#fields.*/(name/*->example::Record#selectDynamic().(name)*/)

type Person/*<-example::RecOrRefined$package.Person#*/ = Record/*->example::Record#*/ {
  val name/*<-local18*/: String/*->scala::Predef.String#*/
  val age/*<-local19*/: Int/*->scala::Int#*/
}

// RecType
class C/*<-example::C#*/ { type T1/*<-example::C#T1#*/; type T2/*<-example::C#T2#*/ }
type C2/*<-example::RecOrRefined$package.C2#*/ = C/*->example::C#*/ { type T1/*<-local20*/; type T2/*<-local21*/ = T1/*->local20*/ }

trait SpecialRefinement/*<-example::SpecialRefinement#*/ {
  def pickOne/*<-example::SpecialRefinement#pickOne().*/[T/*<-example::SpecialRefinement#pickOne().[T]*/](as/*<-example::SpecialRefinement#pickOne().(as)*/: T/*->example::SpecialRefinement#pickOne().[T]*/*): Option/*->scala::Option#*/[Any/*->scala::Any#*/]
}

class PickOneRefinement_1/*<-example::PickOneRefinement_1#*/[S/*<-example::PickOneRefinement_1#[S]*/ <: SpecialRefinement/*->example::SpecialRefinement#*/ { def pickOne/*<-local2*/[T/*<-local0*/](as/*<-local1*/: T/*->local0*/*): Option/*->scala::Option#*/[String/*->scala::Predef.String#*/] }] {
  def run/*<-example::PickOneRefinement_1#run().*/(s/*<-example::PickOneRefinement_1#run().(s)*/: S/*->example::PickOneRefinement_1#[S]*/, as/*<-example::PickOneRefinement_1#run().(as)*/: String/*->scala::Predef.String#*/*): Option/*->scala::Option#*/[String/*->scala::Predef.String#*/] = s/*->example::PickOneRefinement_1#run().(s)*/.pickOne/*->example::SpecialRefinement#pickOne().*/(as/*->example::PickOneRefinement_1#run().(as)*/:_*)
}
