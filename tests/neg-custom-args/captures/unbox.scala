import language.`3.5`
type Proc = () => Unit

val xs: List[Proc] = ???

val x = xs.head // error
