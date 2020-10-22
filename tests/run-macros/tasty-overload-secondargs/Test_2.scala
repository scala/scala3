object Test:

 def main(args:Array[String]):Unit =
    val x1 = X.andThen(1){case x if (x%2 == 0) => x}
    val x2 = Macro.mThen{case x:Int if (x%2 == 0) => x}
