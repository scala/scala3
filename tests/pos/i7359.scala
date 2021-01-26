package test

trait ObjectInterface:
  def equals(obj: Any): Boolean
  def hashCode(): Int
  def toString(): String

trait SAMPlain:
  def first(): String

trait SAMPlainWithOverriddenObjectMethods:
  def first(): String
  def equals(obj: Any): Boolean
  def hashCode(): Int
  def toString(): String

trait SAMPlainWithExtends extends ObjectInterface:
  def first(): String

trait SAMPlainWithExtendsAndOverride extends ObjectInterface:
  def first(): String
  override def equals(obj: Any): Boolean
  override def hashCode(): Int
  override def toString(): String

trait SAMPlainCovariantOut[+O]:
  def first(): O

trait SAMCovariantOut[+O]:
  def first(): O
  def equals(obj: Any): Boolean
  def hashCode(): Int
  def toString(): String

trait SAMCovariantOutExtends[+O] extends ObjectInterface:
  def first(): O

trait SAMCovariantOutExtendsAndOverride[+O] extends ObjectInterface:
  def first(): O
  override def equals(obj: Any): Boolean
  override def hashCode(): Int
  override def toString(): String

trait SAMPlainContravariantIn[-I]:
  def first(in: I): Unit

trait SAMContravariantIn[-I]:
  def first(in: I): Unit
  def equals(obj: Any): Boolean
  def hashCode(): Int
  def toString(): String

trait SAMContravariantInExtends[-I] extends ObjectInterface:
  def first(in: I): Unit

trait SAMContravariantInExtendsAndOverride[-I] extends ObjectInterface:
  def first(in: I): Unit
  override def equals(obj: Any): Boolean
  override def hashCode(): Int
  override def toString(): String

trait SAMPlainInvariant[T]:
  def first(in: T): T

trait SAMInvariant[T]:
  def first(in: T): T
  def equals(obj: Any): Boolean
  def hashCode(): Int
  def toString(): String

trait SAMInvariantExtends[T] extends ObjectInterface:
  def first(in: T): T

trait SAMInvariantExtendsAndOverride[T] extends ObjectInterface:
  def first(in: T): T
  override def equals(obj: Any): Boolean
  override def hashCode(): Int
  override def toString(): String

trait SAMPlainInOut[-I, +O]:
  def first(in: I): O

trait SAMInOut[-I, +O]:
  def first(in: I): O
  def equals(obj: Any): Boolean
  def hashCode(): Int
  def toString(): String

trait SAMInOutExtends[-I, +O] extends ObjectInterface:
  def first(in: I): O

trait SAMInOutExtendsAndOverride[-I, +O] extends ObjectInterface:
  def first(in: I): O
  override def equals(obj: Any): Boolean
  override def hashCode(): Int
  override def toString(): String

type CustomString = String
type CustomBoolean = Boolean
type CustomInt = Int

trait SAMWithCustomAliases:
  def first(): String
  def equals(obj: Any): CustomBoolean
  def hashCode(): CustomInt
  def toString(): CustomString

object Main:
  def main(args: Array[String]) =
    val samPlain : SAMPlain = () => "Hello, World!"
    val samPlainWithOverriddenObjectMethods: SAMPlainWithOverriddenObjectMethods = () => "Hello, World!"
    val samPlainWithExtends : SAMPlainWithExtends = () => "Hello, World!"
    val samPlainWithExtendsAndOverride : SAMPlainWithExtendsAndOverride = () => "Hello, World!"

    val samPlainCovariantOut : SAMPlainCovariantOut[_] = () => "Hello, World!"
    val samCovariantOut : SAMCovariantOut[_] = () => "Hello, World!"
    val samCovariantOutExtends : SAMCovariantOutExtends[_] = () => "Hello, World!"
    val samCovariantOutExtendsAndOverride : SAMCovariantOutExtendsAndOverride[_] = () => "Hello, World!"

    val samPlainContravariantIn : SAMPlainContravariantIn[Int] = (x: Int) => ()
    val samContravariantIn : SAMContravariantIn[Int] = (x: Int) => ()
    val samContravariantInExtends : SAMContravariantInExtends[Int] = (x: Int) => ()
    val samContravariantInExtendsAndOverride : SAMContravariantInExtendsAndOverride[Int] = (x: Int) => ()

    val samPlainInvariant : SAMPlainInvariant[String] = (x: String) => x
    val samInvariant : SAMInvariant[String] = (x: String) => x
    val samInvariantExtends : SAMInvariantExtends[String] = (x: String) => x
    val samInvariantExtendsAndOverride : SAMInvariantExtendsAndOverride[String] = (x: String) => x

    val samPlainInOut : SAMPlainInOut[Int, String] = (x: Int) => x.toString
    val samInOut : SAMInOut[Int, String] = (x: Int) => x.toString
    val samInOutExtends : SAMInOutExtends[Int, String] = (x: Int) => x.toString
    val samInOutExtendsAndOverride : SAMInOutExtendsAndOverride[Int, String] = (x: Int) => x.toString

    val samWithCustomAliases : SAMWithCustomAliases = () => "Hello, World!"
