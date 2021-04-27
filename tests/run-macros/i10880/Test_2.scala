object Test {
  import Dsl.*

  inline def q2 = MyQuoteMacro.myquote(ent.content(MyInsert("Foo")))

  def main(args: Array[String]): Unit = {
    println( PullAst.apply( q2 ) )
  }
}
