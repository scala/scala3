package xsbt

import org.junit.Test
import org.junit.Assert.*

import xsbti.api.Def
import xsbt.api.HashAPI

class ExtraTypeParametersSpecification:
  @Test
  def changesToBounds(): Unit =
    val original = hash("extension (receiver: Int) def choose[A](value: A): A = value")
    val upper = hash("extension (receiver: Int) def choose[A <: CharSequence](value: A): A = value")
    val lower = hash("extension (receiver: Int) def choose[A >: String](value: A): A = value")
    assertNotEquals(original, upper)
    assertNotEquals(original, lower)

  @Test
  def changesToHigherKindedBounds(): Unit =
    val original = hash("def choose(receiver: Int)[F[X]](value: Int): Int = value")
    val changed = hash("def choose(receiver: Int)[F[X <: CharSequence]](value: Int): Int = value")
    assertNotEquals(original, changed)

  @Test
  def positionsOfTypeParameterLists(): Unit =
    val original = hash("def choose(x: Int)[A](y: Int)(z: Int): Int = z")
    val changed = hash("def choose(x: Int)(y: Int)[A](z: Int): Int = z")
    assertNotEquals(original, changed)

  private def hash(method: String): Int =
    val source = s"package repro\n$method"
    val apis = new ScalaCompilerForUnitTesting().extractApiFromSrc(source)
    val definition = apis.iterator.flatMap(_.structure().declared()).collectFirst:
      case method: Def if method.name() == "choose" => method
    HashAPI(definition.get)
