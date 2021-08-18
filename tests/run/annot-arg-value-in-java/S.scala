@main def Test =
  inline val c = "J"
  println(c + ":")
  annots(c).foreach(println)