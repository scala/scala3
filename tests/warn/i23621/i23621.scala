val f2: (x: 42, y: true) = (x = 42, y = true)
val x2 = f2 match  // warn match may not be exhaustive
   case (1, y) => ()

