object JS {

  object dom {
    type Event
    type MouseEvent <: Event
  }

  type EventTypeOf[Tp <: String] <: dom.Event = Tp match {
    case "click" => dom.MouseEvent
    case _ => dom.Event
  }

  def addEventListener[Tp <: String, Ev <: EventTypeOf[Tp]](tpe: Tp)(e: Ev => Any): Unit = ???

  addEventListener("click") { (e: dom.MouseEvent) => ??? }

}
