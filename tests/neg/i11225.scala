import compiletime.uninitialized

@main def Test(x: Boolean) =
  var cached: Int = uninitialized   // error
  cached = if x then 1 else uninitialized  // error
