type Proc = {*} () => Unit

val xs: List[Proc] = ???

val x = xs.head // error
