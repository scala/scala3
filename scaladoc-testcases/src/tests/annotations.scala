package tests
package annotations

import scala.annotation.StaticAnnotation

import java.lang.Enum as _
import scala.reflect.Enum

class SomeObject(val s: String)

class MyAnnotation extends StaticAnnotation

class AnnotationWithArg(val s: String, val o: SomeObject) extends StaticAnnotation

class AnnotationWithMultiArg(val i: Int, val s: String, val c: Char*) extends StaticAnnotation

class EnumAnnotation(val e: Enum) extends StaticAnnotation

class ClassAnnotation[T](val c: Class[T]) extends StaticAnnotation

@AnnotationWithMultiArg(2, "cda", 'a', 'b', 'c') @MyAnnotation class AnnotatedClass


class AnnotatedParams(@MyAnnotation val a: String, @AnnotationWithMultiArg(2, "cda", 'a', 'b', 'c') val b: Int)

class AnnotatedMethods
{
  @MyAnnotation @AnnotationWithMultiArg(2, "cda", 'a', 'b', 'c') def a: String
  = ???
}
