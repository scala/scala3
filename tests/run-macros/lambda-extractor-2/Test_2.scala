
@main def Test = {
  println(test((x, y) => x + y))
  println(test((x, y) => { println(x); y }))
}
