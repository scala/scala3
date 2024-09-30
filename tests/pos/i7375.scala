trait Entity[M, T, P]:
  extension (me: M) def receive(sender: T)(msg: P)(using Entity[T, M, P]): Unit
  extension (me: M) def name(): String

class GreetingPerson(private val name: String)

object GreetingPerson:
  given GreetingPersonEntity: Entity[GreetingPerson, GreetedPerson, String]:
    extension (me: GreetingPerson) def receive(sender: GreetedPerson)(msg: String)(using Entity[GreetedPerson, GreetingPerson, String]): Unit =
       println(f"Thanks for saying $msg, ${sender.name()}")

    extension (me: GreetingPerson) def name() =
      me.name

class GreetedPerson(private val name: String)

object GreetedPerson:
  given GreetedPersonEntity: Entity[GreetedPerson, GreetingPerson, String]:
    extension (me: GreetedPerson) def receive(sender: GreetingPerson)(msg: String)(using Entity[GreetingPerson, GreetedPerson, String]): Unit =
      println(f"Thanks for saying $msg, ${sender.name()}")

    extension (me: GreetedPerson) def name() =
      me.name
