object Test {
  sealed trait Off
  case class Of[sup, sub <: sup]() extends Off
  type Sup[O <: Off] = O match { case Of[sup, sub] => sup }
  type Sub[O <: Off] = O match { case Of[sup, sub] => sub }
  type Copy[O <: Off] = Of[Sup[O], Sub[O]]  // error
}
