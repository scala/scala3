object Test:
  def main(args: Array[String]): Unit =
    println( PullAst.apply( MyQuoteMacro.myquote ) )
