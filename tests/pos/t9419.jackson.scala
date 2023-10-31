// from failure in the community project
// jackson-module-scala
// in ScalaAnnotationIntrospectorModule.scala:139:12

import scala.language.implicitConversions

trait EnrichedType[X]:
  def value: X

trait ClassW extends EnrichedType[Class[?]]:
  def extendsScalaClass = false

class Test:
  implicit def mkClassW(c: => Class[?]): ClassW = new ClassW:
    lazy val value = c

  def test1(c1: Class[?]) = c1.extendsScalaClass           // ok: c1 is a value
  def test2(c2: Class[?]) = mkClassW(c2).extendsScalaClass // ok: c2 is a value
  // c1 in test1 goes throw adapting to find the extension method and gains the wildcard capture cast then
  // c2 in test2 goes straight to typedArg, as it's already an arg, so it never gets wildcard captured
