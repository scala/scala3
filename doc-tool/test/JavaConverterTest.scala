package dotty.tools
package dottydoc

import org.junit.Test
import org.junit.Assert._
import model.{
  Entity,
  Members,
  SuperTypes,
  Modifiers,
  TypeParams,
  Constructors => MConstructors,
  Companion,
  ReturnValue,
  ImplicitlyAddedEntity,
  TypeAlias,
  Trait,
  Def,
  NonEntity
}
import model.references.{ConstantReference, TypeReference, NoLink}
import model.comment.Comment
import dotty.tools.dotc.core.Symbols.NoSymbol
import _root_.java.util.{Optional => JOptional, Map => JMap}

class JavaConverterTest {
  import model.JavaConverters._

  @Test def entityConversions = {
    trait TestEntity extends Entity {
      def symbol = NoSymbol

      def name = "test"

      def kind = "ent"

      def path = "path" :: "to" :: "test" :: Nil

      def comment = Some(new Comment("", "", List(), List(), None, Map(), Map(), Map(), None, None, List(), None, List(), List(), None, None, Map(), Map(), Map(), List()))

      def annotations = List("test")

      def parent = NonEntity

    }
    trait TestMembers extends TestEntity with Members {
      def members = new TestEntity{} :: Nil
    }
    trait TestSuperTypes extends TestEntity with SuperTypes {
      def superTypes = new NoLink("title", "query") :: Nil
    }
    trait TestModifiers extends TestEntity with Modifiers {
      def modifiers = "private" :: Nil
    }
    trait TestTypeParams extends TestEntity with TypeParams {
      def typeParams = "String" :: "String" :: Nil
    }
    trait TestConstructors extends TestEntity with MConstructors {
      def constructors = List(List())
    }
    trait TestCompanion extends TestEntity with Companion {
      def companionPath = "path" :: "to" :: "companion" :: Nil
      def companionPath_=(xs: List[String]) = Unit
    }
    trait TestReturnValue extends TestEntity with ReturnValue {
      def returnValue =
        new TypeReference("String", new NoLink("title", "target"), List())
    }
    trait TestImplicitlyAddedEntity
        extends TestEntity
        with ImplicitlyAddedEntity {
      def implicitlyAddedFrom =
        Some(
          new TypeReference("String", new NoLink("title", "target"), List()))
    }
    trait TestTypeAlias extends TestTypeParams with TestModifiers with TypeAlias {
      override val kind = "type"
      def alias = None
    }
    trait TestDef extends TestModifiers with TestTypeParams with TestImplicitlyAddedEntity {
      def paramLists = List()
    }
    trait TestTrait extends TestModifiers with TestTypeParams with TestSuperTypes with TestMembers with TestCompanion {
      def traitParams = List()
    }
    val ent = new TestEntity {}
    val members = new TestMembers {}
    val superTypes = new TestSuperTypes {}
    val modifiers = new TestModifiers {}
    val typeParams = new TestTypeParams {}
    val constructors = new TestConstructors {}
    val companion = new TestCompanion {}
    val returnValue = new TestReturnValue {}
    val implicitlyAddedEntity = new TestImplicitlyAddedEntity {}
    val typeAlias = new TestTypeAlias {}
    val df = new TestDef {}
    val trt = new TestTrait {}
    val ent_serialized = ent.asJava()
    val members_serialized = members.asJava()
    val superTypes_serialized = superTypes.asJava()
    val modifiers_serialized = modifiers.asJava()
    val typeParams_serialized = typeParams.asJava()
    val constructors_serialized = constructors.asJava()
    val companion_serialized = companion.asJava()
    val returnValue_serialized = returnValue.asJava()
    val implicitlyAddedEntity_serialized = implicitlyAddedEntity.asJava()
    val typeAlias_serialized = typeAlias.asJava()
    val def_serialized = df.asJava()
    val trait_serialized = trt.asJava()
  }
}
