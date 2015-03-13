class projections {

  class Lambda { type Arg; type Apply }

  var x: (Lambda { type Apply = Int; type Arg = String }) # Apply = _
  var y: Int = _
  x = y
  y = x

  var xx: (Lambda { type Apply = Arg } { type Arg = Int }) # Apply = _
  xx = y
  y = xx

}
