enum Labelled {

  case A

  def enumLabel: String = "nolabel" // error: double definition of method enumLabel: => String

}

enum Ordinalled {

  case A

  def ordinal: Int = -1 // error: double definition of method ordinal: => Int

}
