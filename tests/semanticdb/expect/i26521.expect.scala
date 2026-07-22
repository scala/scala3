package example

// https://github.com/scala/scala3/issues/26521
object Foo/*<-example::Foo.*/ {
  def build/*<-example::Foo.build().*/(): Int/*->scala::Int#*/ = {
    final case class Local/*<-local3*/(a/*<-local0*/: Int/*->scala::Int#*/, b/*<-local1*/: Int/*->scala::Int#*/)
    val x/*<-local16*/ = Local/*->local14*/(a/*->local0*/ = 1, b/*->local1*/ = 2)
    x/*->local16*/.a/*->local0*/ +/*->scala::Int#`+`(+4).*/ x/*->local16*/.b/*->local1*/
  }
}
