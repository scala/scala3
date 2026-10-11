// the qualifier must not be lifted, Java statics have no run time value
@main def Test(): Unit =
  p.J.count += 41
  p.J.count += 1
  p.J.count *= 2
  p.J.log += "a"
  p.J.log += "b"
  println(p.J.count)
  println(p.J.log)
