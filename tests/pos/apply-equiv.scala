class Test {

  class Lambda { type Arg; type Apply }

  type T1 = (Lambda { type Arg = Int } { type Apply = List[Arg] }) # Apply
  type T2 = List[Int]

  var x: T1 = _
  var y: T2 = _

  x = y
  y = x

}
