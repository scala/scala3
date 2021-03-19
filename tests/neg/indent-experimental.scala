import language.experimental.fewerBraces

val x =
  if true then:  // error
    1
  else:  // error
    2


val credentials = List("OK")
val all = credentials ++ :  // error
  val file = "file"  // error
  if file.isEmpty    // error
  then Seq("none")
  else Seq(file)
