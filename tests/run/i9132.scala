@main def Test =
  scala.collection.mutable.ArrayBuilder.make[Unit] += ()
  Portable.println ()
  Portable println ()

object Portable:
  def println(x: Any): Unit =
    Console.println(if x == () then "()" else x.toString) // portable on Scala.js
  def println(): Unit =
    Console.println()
