enum Labelled {

  case A // error: method enumLabel of type => String needs `override` modifier

  def enumLabel: String = "nolabel"

}

enum Ordinalled {

  case A // error: method ordinal of type => Int needs `override` modifier

  def ordinal: Int = -1

}
