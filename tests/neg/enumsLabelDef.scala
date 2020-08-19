enum Labelled {

  case A // error overriding method enumLabel in class Labelled of type => String;
  case B(arg: Int) // error overriding method enumLabel in class Labelled of type => String;

  def enumLabel: String = "nolabel"
}

trait Mixin { def enumLabel: String = "mixin" }

enum Mixed extends Mixin {
  case C // error overriding method enumLabel in trait Mixin of type => String;
}

trait HasEnumLabel { def enumLabel: String }

enum MyEnum extends HasEnumLabel {
  case D // ok
}
