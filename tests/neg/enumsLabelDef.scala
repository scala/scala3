enum Labelled {
  case A // error: cannot override final member method enumLabel in class Labelled
  case B(arg: Int) // ok: enumLabel has same behaviour here as productPrefix

  override final def enumLabel: String = "nolabel"
}
