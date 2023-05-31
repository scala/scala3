import language.`3.2`
type Proc = () => Unit

val xs: List[Proc] = ???

val x = xs.head // error
