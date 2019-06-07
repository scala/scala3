
object TestA {
  import Macro._
  foo"""abc${"123"}xyz${"456"}fgh""" // error // error // error
}