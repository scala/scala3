import scala.reflect.Selectable.reflectiveSelectable

object B {
	def onX(m: { def x: Int } ) =
		m.x
}