import scala.annotation.experimental

@experimental type E

type A = E // error
@experimental type B = E
