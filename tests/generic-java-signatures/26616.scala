object MyTrait:
  case class Event[D](value: D)
  case class State[S, D](name: S, data: D)

trait MyTrait[S, D]:
  import MyTrait.*
  type StateT = State[S, D]
  type EventT = Event[D]
  type StateFunction = PartialFunction[EventT, StateT]
  private val handler: StateFunction = { case e => State(null.asInstanceOf[S], e.value) }

abstract class MyAbstract[S, D] extends MyTrait[S, D]

abstract class MyAbstract2[A, B] extends MyTrait[A, B]

object Test:
  def main(args: Array[String]): Unit =
    classOf[MyAbstract[String, String]].getMethods.sortBy(_.getName).filter(_.getName.contains("handler")).foreach(m => {
      println(m)
      println(m.toGenericString)
    })
    classOf[MyAbstract2[String, String]].getMethods.sortBy(_.getName).filter(_.getName.contains("handler")).foreach(m => {
      println(m)
      println(m.toGenericString)
    })

