import scala.quoted.StagingContext
class Foo {
  def foo: Unit = {
    def expr(implicit st: StagingContext) = '{
      val a = 3
      println("foo")
      2 + a
    }
  }
}
