def tu(): (Int, Boolean) = (1, true)

@main def ma(): Unit =
  var x = 0
  var y = false
  var z = "a"

  x = 99
  (x, y) = tu()
  (x, z) = (2, "b")
