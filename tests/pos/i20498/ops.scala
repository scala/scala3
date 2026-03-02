package demo.util
trait Ops:
  final implicit class Ops[A](private val self: A):
    def tap(): Unit = ()
