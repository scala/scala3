class C {
  def m(x: true) = x match { // was: match may not be exhaustive.\nIt would fail on pattern case: false
    case true => println("the one true path")
  }

  def n(x: true) = x match {
    case true => 1
    case false => 2 // was: no reachability warning on this case
  }
}
