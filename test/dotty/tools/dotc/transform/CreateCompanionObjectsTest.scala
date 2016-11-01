package dotty.tools
package dotc
package transform

import org.junit.{Assert, Test}
import core._
import ast.{tpd, Trees}
import Contexts._
import Flags._
import Denotations._
import NameOps._
import Symbols._
import Types._
import Decorators._
import Trees._
import TreeTransforms.{TreeTransform, TreeTransformer}


class CreateCompanionObjectsTest extends DottyTest {
  /* FIXME: re-enable after adapting to new scheme

  import tpd._

  type PostTyperTransformer = TreeTransformer // FIXME do without

  @Test
  def shouldCreateNonExistingObjectsInPackage = checkCompile("frontend", "class A{} ") {
    (tree, context) =>
      implicit val ctx = context

      val transformer = new PostTyperTransformer {
        override def transformations = Array(new CreateCompanionObjects {

          override def name: String = "create all companion objects"
          override def predicate(cts: TypeDef)(implicit ctx:Context): Boolean = true
          init(ctx, ctx.period.firstPhaseId, ctx.period.lastPhaseId)
        })

        override def name: String = "test"
      }
      val transformed = transformer.transform(tree).toString
      val classPattern = "TypeDef(Modifiers(,,List()),A,"
      val classPos = transformed.indexOf(classPattern)
      val moduleClassPattern = "TypeDef(Modifiers(final module <synthetic>,,List()),A$"
      val modulePos = transformed.indexOf(moduleClassPattern)

      Assert.assertTrue("should create non-existing objects in package",
        classPos < modulePos
      )
  }

  @Test
  def shouldCreateNonExistingObjectsInBlock = checkCompile("frontend", "class D {def p = {class A{}; 1}} ") {
    (tree, context) =>
      implicit val ctx = context
      val transformer = new PostTyperTransformer {
        override def transformations = Array(new CreateCompanionObjects {

          override def name: String = "create all companion modules"
          override def predicate(cts: TypeDef)(implicit ctx:Context): Boolean = true
          init(ctx, ctx.period.firstPhaseId, ctx.period.lastPhaseId)
        })

        override def name: String = "test"
      }
      val transformed = transformer.transform(tree).toString
      val classPattern = "TypeDef(Modifiers(,,List()),A,"
      val classPos = transformed.indexOf(classPattern)
      val moduleClassPattern = "TypeDef(Modifiers(final module <synthetic>,,List()),A$"
      val modulePos = transformed.indexOf(moduleClassPattern)

      Assert.assertTrue("should create non-existing objects in block",
        classPos < modulePos
      )
  }

  @Test
  def shouldCreateNonExistingObjectsInTemplate = checkCompile("frontend", "class D {class A{}; } ") {
    (tree, context) =>
      implicit val ctx = context
      val transformer = new PostTyperTransformer {
        override def transformations = Array(new CreateCompanionObjects {
          override def name: String = "create all companion modules"
          override def predicate(cts: TypeDef)(implicit ctx:Context): Boolean = true
          init(ctx, ctx.period.firstPhaseId, ctx.period.lastPhaseId)
        })

        override def name: String = "test"
      }
      val transformed = transformer.transform(tree).toString
      val classPattern = "TypeDef(Modifiers(,,List()),A,"
      val classPos = transformed.indexOf(classPattern)
      val moduleClassPattern = "TypeDef(Modifiers(final module <synthetic>,,List()),A$"
      val modulePos = transformed.indexOf(moduleClassPattern)

      Assert.assertTrue("should create non-existing objects in template",
        classPos < modulePos
      )
  }

  @Test
  def shouldCreateOnlyIfAskedFor = checkCompile("frontend", "class DONT {class CREATE{}; } ") {
    (tree, context) =>
      implicit val ctx = context
      val transformer = new PostTyperTransformer {
        override def transformations = Array(new CreateCompanionObjects {
          override def name: String = "create all companion modules"
          override def predicate(cts: TypeDef)(implicit ctx:Context): Boolean = cts.name.toString.contains("CREATE")
          init(ctx, ctx.period.firstPhaseId, ctx.period.lastPhaseId)
        })

        override def name: String = "test"
      }
      val transformed = transformer.transform(tree).toString
      val classPattern = "TypeDef(Modifiers(,,List()),A,"
      val classPos = transformed.indexOf(classPattern)
      val moduleClassPattern = "TypeDef(Modifiers(final module <synthetic>,,List()),CREATE$"
      val modulePos = transformed.indexOf(moduleClassPattern)

      val notCreatedModulePattern = "TypeDef(Modifiers(final module <synthetic>,,List()),DONT"
      val notCreatedPos = transformed.indexOf(notCreatedModulePattern)

      Assert.assertTrue("should create non-existing objects in template",
        classPos < modulePos && (notCreatedPos < 0)
      )
  }
  */
}
