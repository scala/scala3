package runscala

import scala.annotation.static

@main def foo(): Unit = ()

object MainScala:
  def main(args: Array[String]): Unit = ()

object StaticNoArgs:
  def main(): Unit = ()

object ProtectedObject:
  protected def main(): Unit = ()

class ProtectedStatic

object ProtectedStatic:
  @static protected def main(): Unit = ()

class NoStatic:
  def main(args: Array[String]): Unit = ()

class NoArgs:
  def main(): Unit = ()

class Parameterless:
  def main: Unit = ()

class ConstructorArgument(message: String):
  def main(): Unit = ()

class PrivateConstructor private ():
  def main(): Unit = ()

class SecondaryNoArgs(message: String):
  def this() = this("")
  def main(): Unit = ()

class Protected:
  protected def main(): Unit = ()

class Private:
  private def main(): Unit = ()

class Outer:
  class Inner:
    def main(): Unit = ()

trait Tr:
  def main(): Unit = ()

class InheritTrait extends Tr

abstract class Abstract:
  def main(): Unit = ()

class NonUnit:
  def main(): Int = 1

class ParameterlessNonUnit:
  def main: Int = 1
