import compiletime.uninitialized
class projections {

  class Lambda { type Arg; type Apply }

  var x: (Lambda { type Apply = Int; type Arg = String }) # Apply = uninitialized
  var y: Int = uninitialized
  x = y
  y = x

  var xx: (Lambda { type Apply = Arg } { type Arg = Int }) # Apply = uninitialized
  xx = y
  y = xx

}
