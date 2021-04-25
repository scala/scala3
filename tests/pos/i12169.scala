class Property[T]

class VObject {
  def properties() = {
    List.empty[Property[?]].collect {
      case p: Property[?] => List(p)
    }
  }
}

class Event extends VObject {
  override def properties() = ???
}