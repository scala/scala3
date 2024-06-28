//> using options  -deprecation

def usered = Color.Red // warn: value Red is deprecated

object DeprecatedContainer {
  @deprecated("no foo", "0.1") val foo = 23
}

enum Day {

  @deprecated("no more Mondays!", "0.1") case Monday

}

enum Color {

  @deprecated("no Red", "0.1") case Red

  @deprecated("no Generic", "0.1") case Generic(rgb: Int)

  def useFoo1 = DeprecatedContainer.foo // warn // check that only enum cases are avoided
  def useMonday = Day.Monday // warn // check that enum cases are declared in this enum

}

object Color {
  def useFoo2 = DeprecatedContainer.foo // warn // check that only enum cases are avoided
}