package tests
package annotations

import scala.annotation.StaticAnnotation

import java.lang.Enum as _
import scala.reflect.Enum

class SomeObject(val s: String)

@java.lang.annotation.Documented
class MyAnnotation extends StaticAnnotation //expected: @Documented class MyAnnotation extends StaticAnnotation

@java.lang.annotation.Documented
class AnnotationWithArg(val s: String, val o: SomeObject) extends StaticAnnotation //expected: @Documented class AnnotationWithArg(val s: String, val o: SomeObject) extends StaticAnnotation

@java.lang.annotation.Documented
class AnnotationWithMultiArg(val i: Int, val s: String, val c: Char*) extends StaticAnnotation //expected: @Documented class AnnotationWithMultiArg(val i: Int, val s: String, val c: Char*) extends StaticAnnotation

@java.lang.annotation.Documented
class EnumAnnotation(val e: Enum) extends StaticAnnotation //expected: @Documented class EnumAnnotation(val e: Enum) extends StaticAnnotation

@java.lang.annotation.Documented
class ClassAnnotation[T](val c: Class[T]) extends StaticAnnotation //expected: @Documented class ClassAnnotation[T](val c: Class[T]) extends StaticAnnotation

class NotDocumentedAnnotation extends StaticAnnotation

@AnnotationWithMultiArg(2, "cda", 'a', 'b', 'c') @MyAnnotation class AnnotatedClass


class AnnotatedParams(@MyAnnotation val a: String, @AnnotationWithMultiArg(2, "cda", 'a', 'b', 'c') val b: Int)

class AnnotatedMethods
{
  @MyAnnotation @AnnotationWithMultiArg(2, "cda", 'a', 'b', 'c') def a: String
  = ???
}

/*<-*/@NotDocumentedAnnotation/*->*/class ClassWithoutAnnotation(/*<-*/@NotDocumentedAnnotation/*->*/val a: String)
