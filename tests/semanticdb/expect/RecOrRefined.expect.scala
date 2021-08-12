package example

def m1/*<-example::RecOrRefined$package.m1().*/(a/*<-example::RecOrRefined$package.m1().(a)*/: Int/*->scala::Int#*/ { val x/*<-local1*/: Int/*->scala::Int#*/ }) = ???/*->scala::Predef.`???`().*/
def m2/*<-example::RecOrRefined$package.m2().*/(x/*<-example::RecOrRefined$package.m2().(x)*/: { val x/*<-local2*/: Int/*->scala::Int#*/; def y/*<-local3*/: Int/*->scala::Int#*/ }) = ???/*->scala::Predef.`???`().*/
def m3/*<-example::RecOrRefined$package.m3().*/(x/*<-example::RecOrRefined$package.m3().(x)*/: { val x/*<-local4*/: Int/*->scala::Int#*/; def y/*<-local5*/: Int/*->scala::Int#*/; type z/*<-local6*/ }) = ???/*->scala::Predef.`???`().*/
trait PolyHolder/*<-example::PolyHolder#*/ {
  def foo/*<-example::PolyHolder#foo().*/[T/*<-example::PolyHolder#foo().[T]*/](t/*<-example::PolyHolder#foo().(t)*/: T/*->example::PolyHolder#foo().[T]*/): Any/*->scala::Any#*/
}

def m4/*<-example::RecOrRefined$package.m4().*/(x/*<-example::RecOrRefined$package.m4().(x)*/: PolyHolder/*->example::PolyHolder#*/ { def foo/*<-local9*/[T/*<-local7*/](t/*<-local8*/: T/*->local7*/): T/*->local7*/ }) = ???/*->scala::Predef.`???`().*/
def m5/*<-example::RecOrRefined$package.m5().*/[Z/*<-example::RecOrRefined$package.m5().[Z]*/](x/*<-example::RecOrRefined$package.m5().(x)*/: Int/*->scala::Int#*/): PolyHolder/*->example::PolyHolder#*/ { def foo/*<-local12*/[T/*<-local10*/](t/*<-local11*/: T/*->local10*/): T/*->local10*/ } = ???/*->scala::Predef.`???`().*/

type m6/*<-example::RecOrRefined$package.m6#*/ = [X/*<-example::RecOrRefined$package.m6#[X]*/] =>> PolyHolder/*->example::PolyHolder#*/ { def foo/*<-local15*/[T/*<-local13*/](t/*<-local14*/: T/*->local13*/): T/*->local13*/ }

class Record/*<-example::Record#*/(elems/*<-example::Record#elems.*/: (String/*->scala::Predef.String#*/, Any/*->scala::Any#*/)*) extends Selectable/*->scala::Selectable#*/:
  private val fields/*<-example::Record#fields.*/ = elems/*->example::Record#elems.*/.toMap/*->scala::collection::IterableOnceOps#toMap().*/
  def selectDynamic/*<-example::Record#selectDynamic().*/(name/*<-example::Record#selectDynamic().(name)*/: String/*->scala::Predef.String#*/): Any/*->scala::Any#*/ = fields/*->example::Record#fields.*/(name/*->example::Record#selectDynamic().(name)*/)

type Person/*<-example::RecOrRefined$package.Person#*/ = Record/*->example::Record#*/ {
  val name/*<-local16*/: String/*->scala::Predef.String#*/
  val age/*<-local17*/: Int/*->scala::Int#*/
}

// RecType
class C/*<-example::C#*/ { type T1/*<-example::C#T1#*/; type T2/*<-example::C#T2#*/ }
type C2/*<-example::RecOrRefined$package.C2#*/ = C/*->example::C#*/ { type T1/*<-local18*/; type T2/*<-local19*/ = T1/*->local18*/ }

trait SpecialRefinement/*<-example::SpecialRefinement#*/ {
  def pickOne/*<-example::SpecialRefinement#pickOne().*/[T/*<-example::SpecialRefinement#pickOne().[T]*/](as/*<-example::SpecialRefinement#pickOne().(as)*/: T/*->example::SpecialRefinement#pickOne().[T]*/*): Option/*->scala::Option#*/[Any/*->scala::Any#*/]
}

class PickOneRefinement_1/*<-example::PickOneRefinement_1#*/[S/*<-example::PickOneRefinement_1#[S]*/ <: SpecialRefinement { def pickOne[T](as: T*): Option[String] }] {
  def run/*<-example::PickOneRefinement_1#run().*/(s/*<-example::PickOneRefinement_1#run().(s)*/: S/*->example::PickOneRefinement_1#[S]*/, as/*<-example::PickOneRefinement_1#run().(as)*/: String/*->scala::Predef.String#*/*): Option/*->scala::Option#*/[String/*->scala::Predef.String#*/] = s/*->example::PickOneRefinement_1#run().(s)*/.pickOne/*->example::SpecialRefinement#pickOne().*/(as/*->example::PickOneRefinement_1#run().(as)*/:_*)
}
