
object TestA {
  import Macro.*
  foo"abc${"123"}xyz${"456"}fgh" // error // error // error
}