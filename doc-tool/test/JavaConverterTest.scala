package dotty.tools
package dottydoc

import org.junit.Test
import org.junit.Assert._

import model.{
  Val,
  Object => EObject,
  CaseClass,
  Entity,
  Members,
  SuperTypes,
  Modifiers,
  TypeParams,
  Constructors => EConstructors,
  Class,
  Companion,
  ReturnValue,
  ImplicitlyAddedEntity,
  TypeAlias,
  Trait,
  Package,
  Def,
  NonEntity,
  ParamList
}
import model.references._
import model.internal.{ParamListImpl}
import model.comment.Comment
import dotty.tools.dotc.core.Symbols.NoSymbol
import java.util.{Optional => JOptional, Map => JMap, List => JList}

class JavaConverterTest {
  import model.JavaConverters._
  import scala.collection.JavaConverters._

  @Test def entityConversions = {
    val paramList = new ParamListImpl(new NamedReference("x", new TypeReference("Int", new NoLink("title", "target"), List())) :: Nil, false)
    val df = new Def {
      def symbol = NoSymbol
      def name = "test"
      def path = "path" :: "to" :: "def" :: Nil
      def comment = None
      def annotations = List("test")
      def parent = NonEntity
      def modifiers = "private" :: Nil
      def typeParams = "String" :: "String" :: Nil
      def implicitlyAddedFrom = Some(
          new TypeReference("String", new NoLink("title", "target"), List()))
      def returnValue = new TypeReference("String", new NoLink("title", "target"), List())
      def paramLists = List(paramList)
    }
    assertSerializedCorrectly(df, df.asJava)
    val trt = new Trait {
      def symbol = NoSymbol
      def name = "someTrait"
      def path = "path" :: "to" :: "trait" :: Nil
      def comment = None
      def annotations = List("test")
      def parent = NonEntity
      def modifiers = "protected" :: Nil
      def typeParams = "String" :: "String" :: Nil
      def superTypes = new NoLink("title", "query") :: Nil
      def members = df :: Nil
      def traitParams = List(paramList)
      def companionPath = "path" :: "to" :: "companion" :: Nil
      def companionPath_=(xs: List[String]) = Unit
    }
    assertSerializedCorrectly(trt, trt.asJava)
    val cls = new Class {
      def symbol = NoSymbol
      def name = "test"
      def path = "path" :: "to" :: "test" :: Nil
      def comment = None
      def annotations = List("test")
      def parent = NonEntity
      def modifiers = "private" :: Nil
      def typeParams = "String" :: "String" :: Nil
      def superTypes = new NoLink("title", "query") :: Nil
      def members = Nil
      def companionPath = "path" :: "to" :: "companion" :: Nil
      def companionPath_=(xs: List[String]) = Unit
      def constructors = List(List(paramList))
    }
    assertSerializedCorrectly(cls, cls.asJava)
    val caseClass = new CaseClass {
      def symbol = NoSymbol
      def name = "test"
      def path = "path" :: "to" :: "test" :: Nil
      def comment = None
      def annotations = List("test")
      def parent = NonEntity
      def modifiers = "private" :: Nil
      def typeParams = "String" :: "String" :: Nil
      def constructors = List(List(paramList))
      def superTypes = new NoLink("title", "query") :: Nil
      def members = Nil
      def companionPath = "path" :: "to" :: "companion" :: Nil
      def companionPath_=(xs: List[String]) = Unit
    }
    assertSerializedCorrectly(caseClass, caseClass.asJava)
    val obj = new EObject {
      def symbol = NoSymbol
      def name = "someObject"
      def path = "path" :: "to" :: "object" :: Nil
      def comment = None
      def annotations = List("test")
      def parent = NonEntity
      def modifiers = "protected" :: Nil
      def typeParams = "String" :: "String" :: Nil
      def superTypes = new NoLink("title", "query") :: Nil
      def members = df :: Nil
      def companionPath = "path" :: "to" :: "companion" :: Nil
      def companionPath_=(xs: List[String]) = Unit
    }
    assertSerializedCorrectly(obj, obj.asJava)
    val typeAlias = new TypeAlias {
      def symbol = NoSymbol
      def name = "typeAlias"
      def path = "path" :: "to" :: "typeAlias" :: Nil
      def comment = None
      def annotations = List("test")
      def parent = NonEntity
      def modifiers = "private" :: Nil
      def typeParams = "String" :: "String" :: Nil
      def alias = Some(new TypeReference("String", new NoLink("title", "target"), List()))
    }
    assertSerializedCorrectly(typeAlias, typeAlias.asJava)
    val vl = new Val {
      val kind = "val"
      def symbol = NoSymbol
      def name = "val"
      def path = "path" :: "to" :: "val" :: Nil
      def comment = None
      def annotations = List("test")
      def parent = NonEntity
      def modifiers = "private" :: Nil
      def returnValue = new TypeReference("String", new NoLink("title", "target"), List())
      def implicitlyAddedFrom = Some(
        new TypeReference("String", new NoLink("title", "target"), List()))
    }
    assertSerializedCorrectly(vl, vl.asJava)
    val pkg = new Package {
      def symbol = NoSymbol
      def name = "test"
      def path = "path" :: "to" :: "test" :: Nil
      def comment = None
      def annotations = List("test")
      def parent = NonEntity
      def members = trt :: typeAlias :: Nil
      def superTypes = new NoLink("title", "query") :: Nil
    }
    assertSerializedCorrectly(pkg, pkg.asJava)
  }

  def assertEach[E, C[E] <: Seq[E]](expected: C[E], serialized: Any)(pairwiseAssertion: (E, Any) => Unit): Unit = {
    val s = serialized.asInstanceOf[JList[_]]
    val actual = s.asScala.toList
    assertEquals(expected.length, actual.length)
    for ((exp, act) <- expected zip actual) {
      pairwiseAssertion(exp, act)
    }
  }
  def assertSameSeq[T, C[T] <: Seq[T]](expected: C[T], serialized: Any): Unit = {
    val actual = serialized.asInstanceOf[java.util.List[T]].asScala.toList
    assertEquals(expected, actual);
  }
  def assertSerializedCorrectly(expected: ParamList, serialized: Any): Unit = {
    val actual = serialized.asInstanceOf[JMap[String, _]]
    assertEach(expected.list, actual.get("list")) {(exp, act) =>
      assertSerializedCorrectly(exp, act)
    }
    assertEquals(expected.isImplicit, actual.get("isImplicit"))
  }
  def assertSerializedCorrectly(expected: Reference, serialized: Any): Unit = {
    val actual = serialized.asInstanceOf[JMap[String, _]]
    expected match {
      case TypeReference(title, tpeLink, paramLinks) =>
        assertEquals(title, actual.get("title"))
        assertSerializedCorrectly(tpeLink, actual.get("tpeLink"))
        assertEach(paramLinks, actual.get("paramLinks")) { (exp, act) =>
          assertSerializedCorrectly(exp, act)
        }
      case OrTypeReference(left, right) =>
        assertSerializedCorrectly(left, actual.get("left"))
        assertSerializedCorrectly(right, actual.get("right"))
      case AndTypeReference(left, right) =>
        assertSerializedCorrectly(left, actual.get("left"))
        assertSerializedCorrectly(right, actual.get("right"))
      case FunctionReference(args, returnValue) =>
        assertEach(args, actual.get("args")) { (exp, act) =>
          assertSerializedCorrectly(exp, act)
        }
        assertSerializedCorrectly(returnValue, actual.get("returnValue"))
      case TupleReference(args) =>
        assertEach(args, actual.get("args")) { (exp, act) =>
          assertSerializedCorrectly(exp, act)
        }
      case BoundsReference(low, high) =>
        assertSerializedCorrectly(low, actual.get("low"))
        assertSerializedCorrectly(high, actual.get("high"))
      case NamedReference(title, ref, isByName, isRepeated) =>
        assertEquals(title, actual.get("title"))
        assertSerializedCorrectly(ref, actual.get("ref"))
        assertEquals(isByName, actual.get("isByName"))
        assertEquals(isRepeated, actual.get("isRepeated"))
      case ConstantReference(title) =>
        assertEquals(title, actual.get("title"))
      case EmptyReference =>
    }
  }
  def assertSerializedCorrectly(expected: MaterializableLink, serialized: Any): Unit = {
    val actual = serialized.asInstanceOf[JMap[String, _]]
    expected match {
      case UnsetLink(title, query) =>
        assertEquals(title, actual.get("title"))
        assertEquals(query, actual.get("query"))
      case MaterializedLink(title, target) =>
        assertEquals(title, actual.get("title"))
        assertEquals(target, actual.get("target"))
      case NoLink(title, target) =>
        assertEquals(title, actual.get("title"))
        assertEquals(target, actual.get("target"))
    }
  }
  def assertSerializedCorrectly(expected: Entity, serialized: Any): Unit = {
    val actual = serialized.asInstanceOf[JMap[String, _]]
    assertEquals(expected.name, actual.get("name"))
    assertEquals(expected.kind, actual.get("kind"))
    assertSameSeq(expected.annotations, actual.get("annotations"))
    assertSameSeq(expected.path, actual.get("path"))
    // Only test if a comment is present
    expected.comment match {
      case Some(c) => assertNotEquals(null, actual.get("comment"))
      case _ =>
    }
    expected match {
      case e: Members => {
        assertEach(e.members, actual.get("members")) { (exp, act) =>
          assertSerializedCorrectly(exp, act)
        }
      }
      case _ =>
    }
    expected match {
      case e: SuperTypes => {
        assertEach(e.superTypes, actual.get("superTypes")) { (exp, act) =>
          assertSerializedCorrectly(exp, act)
        }
      }
      case _ =>
    }
    expected match {
      case e: Modifiers => {
        assertSameSeq(e.modifiers, actual.get("modifiers"))
        assertEquals(e.isPrivate, actual.get("isPrivate"))
        assertEquals(e.isProtected, actual.get("isProtected"))
      }
      case _ =>
    }
    expected match {
      case e: TypeParams => {
        assertSameSeq(e.typeParams, actual.get("typeParams"))
      }
      case _ =>
    }
    expected match {
      case e: EConstructors => {
        // constructors is of type List[List[ParamList]], so we need to apply assertEach twice
        assertEach(e.constructors, actual.get("constructors")) { (exp, act) =>
          assertEach(exp, act) { (exp, act) =>
            assertSerializedCorrectly(exp, act)
          }
        }
      }
      case _ =>
    }
    expected match {
      case e: Companion => {
        assertSameSeq(e.companionPath, actual.get("companionPath"))
      }
      case _ =>
    }
    expected match {
      case e: ReturnValue => {
        assertSerializedCorrectly(e.returnValue, actual.get("returnValue"))
      }
      case _ =>
    }
    expected match {
      case e: ImplicitlyAddedEntity => {
        e.implicitlyAddedFrom.map(assertSerializedCorrectly(_, actual.get("implicitlyAddedFrom")))
      }
      case _ =>
    }
    expected match {
      case e: TypeAlias => {
        e.alias.map(assertSerializedCorrectly(_, actual.get("alias")))
      }
      case _ =>
    }
    expected match {
      case e: Def => {
        assertEach(e.paramLists, actual.get("paramLists")) { (exp, act) =>
          assertSerializedCorrectly(exp, act)
        }
      }
      case _ =>
    }
    expected match {
      case e: Trait => {
        assertEach(e.traitParams, actual.get("traitParams")) { (exp, act) =>
          assertSerializedCorrectly(exp, act)
        }
      }
      case _ =>
    }
  }
}
