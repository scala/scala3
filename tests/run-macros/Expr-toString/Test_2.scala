@main def Test() =
  println(showToString(1))
  println(showToString("abc"))
  println(showToString(println("abc")))
  println(showToString { while true do () } )
