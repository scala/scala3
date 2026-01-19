package tests.overloadedMethods

import scala.collection.mutable.Buffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.Seq
import scala.collection.mutable.Iterable
import scala.collection.Iterator
import scala.collection.immutable.List
import java.util.Properties

/**
 * Test class with overloaded methods for testing link resolution.
 */
class OverloadedMethods:
  /**
   * Overloaded method with Buffer parameter.
   */
  def processBuffer[A](b: Buffer[A]): Unit = ???

  /**
   * Overloaded method with Map parameter.
   */
  def processMap[K, V](m: Map[K, V]): Unit = ???

  /**
   * Overloaded method with Set parameter.
   */
  def processSet[A](s: Set[A]): Unit = ???

  /**
   * Overloaded method with Seq parameter.
   */
  def processSeq[A](s: Seq[A]): Unit = ???

  /**
   * Overloaded method with Iterator parameter.
   */
  def processIterator[A](it: Iterator[A]): Unit = ???

  /**
   * Overloaded method with Iterable parameter.
   */
  def processIterable[A](it: Iterable[A]): Unit = ???

  /**
   * Overloaded method with List parameter.
   */
  def processList[A](l: List[A]): Unit = ???

  /**
   * Overloaded method with Properties parameter.
   */
  def processProperties(p: Properties): Unit = ???

  /**
   * Method with multiple overloads - simple version.
   */
  def transform(x: Int): Int = x * 2

  /**
   * Method with multiple overloads - Buffer version.
   */
  def transform[A](b: Buffer[A]): Buffer[A] = b

  /**
   * Method with multiple overloads - Map version.
   */
  def transform[K, V](m: Map[K, V]): Map[K, V] = m

  /**
   * Method with multiple overloads - Set version.
   */
  def transform[A](s: Set[A]): Set[A] = s

  /**
   * Method with multiple overloads - Seq version.
   */
  def transform[A](s: Seq[A]): Seq[A] = s

  /**
   * Method with multiple overloads - Iterator version.
   */
  def transform[A](it: Iterator[A]): Iterator[A] = it

  /**
   * Method with multiple overloads - Iterable version.
   */
  def transform[A](it: Iterable[A]): Iterable[A] = it

  /**
   * Method with multiple overloads - List version.
   */
  def transform[A](l: List[A]): List[A] = l

  /**
   * Method with multiple overloads - Properties version.
   */
  def transform(p: Properties): Properties = p

/**
 * Test object with overloaded methods.
 */
object OverloadedMethods:
  /**
   * Static overloaded method with Buffer parameter.
   */
  def asJava[A](b: Buffer[A]): java.util.List[A] = ???

  /**
   * Static overloaded method with Map parameter.
   */
  def asJava[K, V](m: Map[K, V]): java.util.Map[K, V] = ???

  /**
   * Static overloaded method with Set parameter.
   */
  def asJava[A](s: Set[A]): java.util.Set[A] = ???

  /**
   * Static overloaded method with Seq parameter.
   */
  def asJava[A](s: Seq[A]): java.util.List[A] = ???

  /**
   * Static overloaded method with Iterator parameter.
   */
  def asJava[A](it: Iterator[A]): java.util.Iterator[A] = ???

  /**
   * Static overloaded method with Iterable parameter.
   */
  def asJava[A](it: Iterable[A]): java.util.Collection[A] = ???

  /**
   * Static overloaded method with List parameter.
   */
  def asJava[A](l: List[A]): java.util.List[A] = ???

  /**
   * Static overloaded method with Properties parameter.
   */
  def asJava(p: Properties): java.util.Map[AnyRef, AnyRef] = ???

/**
 * Test class with links to overloaded methods.
 * These should resolve correctly to the specific overloads.
 */
class OverloadedLinks:
  /**
   * Link to processBuffer overload.
   * [[tests.overloadedMethods.OverloadedMethods.processBuffer[A](b:scala.collection.mutable.Buffer[A])*]]
   */
  def testBufferLink: Unit = ???

  /**
   * Link to processMap overload.
   * [[tests.overloadedMethods.OverloadedMethods.processMap[K,V](m:scala.collection.mutable.Map[K,V])*]]
   */
  def testMapLink: Unit = ???

  /**
   * Link to processSet overload.
   * [[tests.overloadedMethods.OverloadedMethods.processSet[A](s:scala.collection.mutable.Set[A])*]]
   */
  def testSetLink: Unit = ???

  /**
   * Link to processSeq overload.
   * [[tests.overloadedMethods.OverloadedMethods.processSeq[A](s:scala.collection.mutable.Seq[A])*]]
   */
  def testSeqLink: Unit = ???

  /**
   * Link to processIterator overload.
   * [[tests.overloadedMethods.OverloadedMethods.processIterator[A](it:scala.collection.Iterator[A])*]]
   */
  def testIteratorLink: Unit = ???

  /**
   * Link to processIterable overload.
   * [[tests.overloadedMethods.OverloadedMethods.processIterable[A](it:scala.collection.mutable.Iterable[A])*]]
   */
  def testIterableLink: Unit = ???

  /**
   * Link to processList overload.
   * [[tests.overloadedMethods.OverloadedMethods.processList[A](l:scala.collection.immutable.List[A])*]]
   */
  def testListLink: Unit = ???

  /**
   * Link to processProperties overload.
   * [[tests.overloadedMethods.OverloadedMethods.processProperties(p:java.util.Properties)*]]
   */
  def testPropertiesLink: Unit = ???

  /**
   * Link to transform overload with Buffer.
   * [[tests.overloadedMethods.OverloadedMethods.transform[A](b:scala.collection.mutable.Buffer[A])*]]
   */
  def testTransformBufferLink: Unit = ???

  /**
   * Link to transform overload with Map.
   * [[tests.overloadedMethods.OverloadedMethods.transform[K,V](m:scala.collection.mutable.Map[K,V])*]]
   */
  def testTransformMapLink: Unit = ???

  /**
   * Link to transform overload with Set.
   * [[tests.overloadedMethods.OverloadedMethods.transform[A](s:scala.collection.mutable.Set[A])*]]
   */
  def testTransformSetLink: Unit = ???

  /**
   * Link to transform overload with Seq.
   * [[tests.overloadedMethods.OverloadedMethods.transform[A](s:scala.collection.mutable.Seq[A])*]]
   */
  def testTransformSeqLink: Unit = ???

  /**
   * Link to transform overload with Iterator.
   * [[tests.overloadedMethods.OverloadedMethods.transform[A](it:scala.collection.Iterator[A])*]]
   */
  def testTransformIteratorLink: Unit = ???

  /**
   * Link to transform overload with Iterable.
   * [[tests.overloadedMethods.OverloadedMethods.transform[A](it:scala.collection.mutable.Iterable[A])*]]
   */
  def testTransformIterableLink: Unit = ???

  /**
   * Link to transform overload with List.
   * [[tests.overloadedMethods.OverloadedMethods.transform[A](l:scala.collection.immutable.List[A])*]]
   */
  def testTransformListLink: Unit = ???

  /**
   * Link to transform overload with Properties.
   * [[tests.overloadedMethods.OverloadedMethods.transform(p:java.util.Properties)*]]
   */
  def testTransformPropertiesLink: Unit = ???

  /**
   * Link to static asJava overload with Buffer.
   * [[tests.overloadedMethods.OverloadedMethods.asJava[A](b:scala.collection.mutable.Buffer[A])*]]
   */
  def testAsJavaBufferLink: Unit = ???

  /**
   * Link to static asJava overload with Map.
   * [[tests.overloadedMethods.OverloadedMethods.asJava[K,V](m:scala.collection.mutable.Map[K,V])*]]
   */
  def testAsJavaMapLink: Unit = ???

  /**
   * Link to static asJava overload with Set.
   * [[tests.overloadedMethods.OverloadedMethods.asJava[A](s:scala.collection.mutable.Set[A])*]]
   */
  def testAsJavaSetLink: Unit = ???

  /**
   * Link to static asJava overload with Seq.
   * [[tests.overloadedMethods.OverloadedMethods.asJava[A](s:scala.collection.mutable.Seq[A])*]]
   */
  def testAsJavaSeqLink: Unit = ???

  /**
   * Link to static asJava overload with Iterator.
   * [[tests.overloadedMethods.OverloadedMethods.asJava[A](it:scala.collection.Iterator[A])*]]
   */
  def testAsJavaIteratorLink: Unit = ???

  /**
   * Link to static asJava overload with Iterable.
   * [[tests.overloadedMethods.OverloadedMethods.asJava[A](it:scala.collection.mutable.Iterable[A])*]]
   */
  def testAsJavaIterableLink: Unit = ???

  /**
   * Link to static asJava overload with List.
   * [[tests.overloadedMethods.OverloadedMethods.asJava[A](l:scala.collection.immutable.List[A])*]]
   */
  def testAsJavaListLink: Unit = ???

  /**
   * Link to static asJava overload with Properties.
   * [[tests.overloadedMethods.OverloadedMethods.asJava(p:java.util.Properties)*]]
   */
  def testAsJavaPropertiesLink: Unit = ???

  /**
   * Link to simple transform overload (no collection parameter).
   * [[tests.overloadedMethods.OverloadedMethods.transform(x:Int)*]]
   */
  def testTransformSimpleLink: Unit = ???

// Custom types for testing non-collection type overload resolution

/**
 * A custom type for testing overload resolution with non-collection types.
 */
class CustomTypeA(val value: Int)

/**
 * Another custom type for testing overload resolution.
 */
class CustomTypeB(val name: String)

/**
 * Test class with overloaded methods using custom (non-collection) types.
 * This tests that overload resolution works with any types, not just hardcoded collections.
 */
class CustomTypeOverloads:
  /**
   * Process method overloaded with CustomTypeA.
   */
  def process(x: CustomTypeA): Unit = ???

  /**
   * Process method overloaded with CustomTypeB.
   */
  def process(x: CustomTypeB): Unit = ???

  /**
   * Process method overloaded with Int.
   */
  def process(x: Int): Unit = ???

  /**
   * Process method overloaded with String.
   */
  def process(x: String): Unit = ???

/**
 * Test class with overloaded methods that have multiple parameters.
 * This tests that overload resolution correctly matches ALL parameter types, not just one.
 */
class MultiParamOverloads:
  /**
   * Merge method with Buffer and Set parameters.
   */
  def merge[A](a: Buffer[A], b: Set[A]): Unit = ???

  /**
   * Merge method with Set and Buffer parameters (reversed order).
   */
  def merge[A](a: Set[A], b: Buffer[A]): Unit = ???

  /**
   * Merge method with two Maps.
   */
  def merge[K, V](a: Map[K, V], b: Map[K, V]): Unit = ???

  /**
   * Combine method with Int and String.
   */
  def combine(a: Int, b: String): Unit = ???

  /**
   * Combine method with String and Int (reversed order).
   */
  def combine(a: String, b: Int): Unit = ???

  /**
   * Combine method with three parameters.
   */
  def combine(a: Int, b: String, c: Boolean): Unit = ???

/**
 * Test class with links to custom type overloads.
 */
class CustomTypeLinks:
  /**
   * Link to process with CustomTypeA.
   * [[tests.overloadedMethods.CustomTypeOverloads.process(x:tests.overloadedMethods.CustomTypeA)*]]
   */
  def testCustomTypeALink: Unit = ???

  /**
   * Link to process with CustomTypeB.
   * [[tests.overloadedMethods.CustomTypeOverloads.process(x:tests.overloadedMethods.CustomTypeB)*]]
   */
  def testCustomTypeBLink: Unit = ???

/**
 * Test class with links to multi-parameter overloads.
 */
class MultiParamLinks:
  /**
   * Link to merge with Buffer and Set.
   * [[tests.overloadedMethods.MultiParamOverloads.merge[A](a:scala.collection.mutable.Buffer[A],b:scala.collection.mutable.Set[A])*]]
   */
  def testMergeBufferSetLink: Unit = ???

  /**
   * Link to merge with Set and Buffer (reversed).
   * [[tests.overloadedMethods.MultiParamOverloads.merge[A](a:scala.collection.mutable.Set[A],b:scala.collection.mutable.Buffer[A])*]]
   */
  def testMergeSetBufferLink: Unit = ???

  /**
   * Link to combine with Int and String.
   * [[tests.overloadedMethods.MultiParamOverloads.combine(a:Int,b:String)*]]
   */
  def testCombineIntStringLink: Unit = ???

  /**
   * Link to combine with String and Int (reversed).
   * [[tests.overloadedMethods.MultiParamOverloads.combine(a:String,b:Int)*]]
   */
  def testCombineStringIntLink: Unit = ???
