import compiletime.notInitialized

@main def Test(x: Boolean) =
  var cached: Int = notInitialized   // error
  cached = if x then 1 else notInitialized  // error
