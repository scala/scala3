import reflect.Selectable.reflectiveSelectable

type Y = { type T = String; def u(): T }

trait Test {

  val y1: Y
  val y2 = y1.u()
}