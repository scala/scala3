enum Labelled {

  case A

  def enumLabel: String = "nolabel" // error: double definition of method enumLabel: => String

}

enum Ordinalled {

  case A // error: double definition of method ordinal: => Int

  def ordinal: Int = -1

}
