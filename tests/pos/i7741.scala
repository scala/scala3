import scala.language.experimental.erasedDefinitions

class A1 {
    @native private def a: Unit
}
trait A3 {
    erased val a: Int
}