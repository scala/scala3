package dotty.tools
package dotc
package transform

import core._
import ast.Trees


class CreateCompanionObjectsTest extends DottyTest {
  /* FIXME: re-enable after adapting to new scheme

  import tpd._

  type PostTyperTransformer = TreeTransformer // FIXME do without

  @Test
  def shouldCreateNonExistingObjectsInPackage = checkCompile("typer", "class A{} ") {
    (tree, context) =>
      given Context = context

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
  def shouldCreateNonExistingObjectsInBlock = checkCompile("typer", "class D {def p = {class A{}; 1}} ") {
    (tree, context) =>
      given Context = context
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
  def shouldCreateNonExistingObjectsInTemplate = checkCompile("typer", "class D {class A{}; } ") {
    (tree, context) =>
      given Context = context
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
  def shouldCreateOnlyIfAskedFor = checkCompile("typer", "class DONT {class CREATE{}; } ") {
    (tree, context) =>
      given Context = context
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
