class Test {
  import compiletime.uninitialized

  class Lambda { type Arg; type Apply }

  type T1 = (Lambda { type Arg = Int } { type Apply = List[Arg] }) # Apply
  type T2 = List[Int]

  var x: T1 = uninitialized
  var y: T2 = uninitialized

  x = y
  y = x

}
