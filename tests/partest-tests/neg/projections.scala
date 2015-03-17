class projections {

  class Lambda { type Arg; type Apply }

  var x: (Lambda { type Apply = Int }) # Apply = _ // error: illegal prefix

}
