
@main def Test = {
  println(test(identity))
  println(test(x => x))
  println(test(x => { println(x); x }))
}
