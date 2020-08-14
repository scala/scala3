enum Labelled {

  case A // error: cannot override final member method enumLabel in class Labelled
  case B(arg: Int) // error: cannot override final member method enumLabel in class Labelled

  final def enumLabel: String = "nolabel"
}
