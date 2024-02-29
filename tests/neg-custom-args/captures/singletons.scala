val x = () => ()

val y1: x.type = x          // ok
val y2: x.type^{} = x       // error: singleton type cannot have capture set
val y3: x.type^{x} = x      // error: singleton type cannot have capture set // error
val y4: x.type^ = x         // error: singleton type cannot have capture set
