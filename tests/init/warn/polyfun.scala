class Test {
  val m: [T] => (arg: T) => T =
    [T] => (arg: T) => {
      println(n)
      arg
    }
  val n = m.apply(arg = 23)   // warn
}
