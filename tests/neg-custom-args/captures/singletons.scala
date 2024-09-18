val x = () => ()

val y1: x.type = x
val y2: x.type^{} = x
val y3: x.type^{x} = x // error
val y4: x.type^ = x
