import scala.language.experimental.erasedDefinitions

class A1 {
    @native private def a: Unit
}
trait A2 {
    erased def i(a: Int): Int
}
trait A3 {
    erased val a: Int
}