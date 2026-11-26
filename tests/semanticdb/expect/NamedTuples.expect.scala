package example

object NamedTuples/*<-example::NamedTuples.*/:
  type Person/*<-example::NamedTuples.Person#*/ = (name: String/*->scala::Predef.String#*/, age: Int/*->scala::Int#*/)
  val x/*<-example::NamedTuples.x.*/: Person/*->example::NamedTuples.Person#*/ = (name = "a", age = 1)
  val y/*<-example::NamedTuples.y.*/ = x/*->example::NamedTuples.x.*/.name
  val z/*<-example::NamedTuples.z.*/ = x/*->example::NamedTuples.x.*/.apply/*->scala::NamedTuple.apply(+1).*/(0)
  val w/*<-example::NamedTuples.w.*/ = x/*->example::NamedTuples.x.*/ match
    case (name = n/*<-local0*/, age = a/*<-local1*/) => n/*->local0*/
  val v/*<-example::NamedTuples.v.*/ = x/*->example::NamedTuples.x.*/.name.length/*->java::lang::String#length().*/

  case class P/*<-example::NamedTuples.P#*/(name/*<-example::NamedTuples.P#name.*//*<-example::NamedTuples.P#`<init>`().(name)*/: String/*->scala::Predef.String#*/)
  val p/*<-example::NamedTuples.p.*/: NamedTuple/*->scala::NamedTuple.*/.From/*->scala::NamedTuple.From#*/[P/*->example::NamedTuples.P#*/] = ???/*->scala::Predef.`???`().*/
  val q/*<-example::NamedTuples.q.*/ = p/*->example::NamedTuples.p.*/.name
