object Test with
  def main(args: Array[String]): Unit =
    println( PullAst.apply( MyQuoteMacro.myquote ) )
