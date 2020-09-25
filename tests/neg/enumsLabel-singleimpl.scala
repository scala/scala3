enum Labelled {

  case A

  def enumLabel: String = "nolabel" // error: redefinition of method enumLabel: => String in an enum
}

enum Ordinalled {

  case A

  def ordinal: Int = -1 // error: redefinition of method ordinal: => Int in an enum
}
