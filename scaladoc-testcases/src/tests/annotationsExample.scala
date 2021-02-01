package tests
package annotationsExample

import scala.annotation.StaticAnnotation

enum Enum {
  case A extends Enum
  case B extends Enum
  case C extends Enum
}

class SomeObject(val s: String)

class MyAnnotation extends StaticAnnotation

class AnnotationWithArg(val s: String, val o: SomeObject) extends StaticAnnotation

class AnnotationWithMultiArg(val i: Int, val s: String, val c: Char*) extends StaticAnnotation

class EnumAnnotation(val e: Enum) extends StaticAnnotation

class ClassAnnotation[T](val c: Class[T]) extends StaticAnnotation

@MyAnnotation@AnnotationWithMultiArg(2, "cda", 'a', 'b', 'c')@EnumAnnotation(Enum.A)class AnnotatedClass


class AnnotatedParams(@MyAnnotation val a: String, @AnnotationWithMultiArg(2, "cda", 'a', 'b', 'c') val b: Int)

class AnnotatedMethods
{
  @MyAnnotation
  @AnnotationWithMultiArg(2, "cda", 'a', 'b', 'c')
  def a: String
  = ???
}