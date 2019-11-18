@main def Test =
  scala.compiletime.testing.typeCheckErrors("""



            1 + abc



  """).foreach(println)