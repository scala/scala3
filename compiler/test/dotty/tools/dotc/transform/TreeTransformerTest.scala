package dotty.tools
package dotc
package transform

import org.junit.{Assert, Test}
import TreeTransforms.{TransformerInfo, TreeTransformer, MiniPhaseTransform}
import ast.tpd
import core.Constants.Constant
import core.Contexts.Context

class TreeTransformerTest extends DottyTest {

  @Test
  def shouldReturnSameTreeIfUnchanged: Unit = checkCompile("frontend", "class A{ val d = 1}") {
    (tree, context) =>
      implicit val ctx = context
      class EmptyTransform extends MiniPhaseTransform {
        override def phaseName: String = "empty"
        init(ctx, ctx.period.firstPhaseId, ctx.period.lastPhaseId)
      }
      val transformer = new TreeTransformer {
        override def miniPhases = Array(new EmptyTransform)

        override def phaseName: String = "test"
      }
      val transformed = transformer.macroTransform(tree)

      Assert.assertTrue("returns same tree if unmodified",
        tree eq transformed
      )
  }

  // Disabled, awaiting resolution. @Test
  def canReplaceConstant: Unit = checkCompile("frontend", "class A{ val d = 1}") {
    (tree, context) =>
      implicit val ctx = context
      class ConstantTransform extends MiniPhaseTransform {

        override def transformLiteral(tree: tpd.Literal)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = tpd.Literal(Constant(2))
        override def phaseName: String = "canReplaceConstant"
        init(ctx, ctx.period.firstPhaseId, ctx.period.lastPhaseId)
      }
      val transformer = new TreeTransformer {
        override def miniPhases = Array(new ConstantTransform)

        override def phaseName: String = "test"
      }
      val transformed = transformer.macroTransform(tree)

      Assert.assertTrue("returns same tree if unmodified",
        transformed.toString.contains("List(ValDef(Modifiers(,,List()),d,TypeTree[TypeRef(ThisType(module class scala),Int)],Literal(Constant(2)))")
      )
  }

  @Test
  def canOverwrite: Unit = checkCompile("frontend", "class A{ val d = 1}") {
    (tree, context) =>
      implicit val ctx = context
      class Transformation extends MiniPhaseTransform {

        override def transformLiteral(tree: tpd.Literal)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = tpd.Literal(Constant(-1))
        override def phaseName: String = "canOverwrite"

        override def transformValDef(tree: tpd.ValDef)(implicit ctx: Context, info: TransformerInfo): tpd.ValDef = {
          Assert.assertTrue("transformation of children succeeded",
            tree.rhs.toString == "Literal(Constant(-1))"
          )
          tpd.cpy.ValDef(tree)(rhs = tpd.Literal(Constant(2)))
        }

        init(ctx, ctx.period.firstPhaseId, ctx.period.lastPhaseId)
      }
      val transformer = new TreeTransformer {
        override def miniPhases = Array(new Transformation)

        override def phaseName: String = "test"

      }
      val tr = transformer.macroTransform(tree).toString

      Assert.assertTrue("node can rewrite children",
        tr.contains("Literal(Constant(2))") && !tr.contains("Literal(Constant(-1))")
      )
  }

  @Test
  def transformationOrder: Unit = checkCompile("frontend", "class A{ val d = 1}") {
    (tree, context) =>
      implicit val ctx = context
      class Transformation1 extends MiniPhaseTransform {
        override def phaseName: String = "transformationOrder1"

        override def transformLiteral(tree: tpd.Literal)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
          Assert.assertTrue("correct constant",
            tree.const.toString == "Constant(1)"
          )
          tpd.cpy.Literal(tree)(Constant(-1))
        }

        override def transformValDef(tree: tpd.ValDef)(implicit ctx: Context, info: TransformerInfo): tpd.ValDef = {
          Assert.assertTrue("transformation of children succeeded",
            tree.rhs.toString == "Literal(Constant(-1))"
          )
          tpd.cpy.ValDef(tree)(rhs = tpd.Literal(Constant(2)))
        }

        init(ctx, ctx.period.firstPhaseId, ctx.period.lastPhaseId)
      }
      class Transformation2 extends MiniPhaseTransform {
        override def phaseName: String = "transformationOrder2"
        override def transformValDef(tree: tpd.ValDef)(implicit ctx: Context, info: TransformerInfo): tpd.ValDef = {
          Assert.assertTrue("transformation of children succeeded",
            tree.rhs.toString == "Literal(Constant(2))"
          )
          tpd.cpy.ValDef(tree)(rhs = tpd.Literal(Constant(3)))
        }

        init(ctx, ctx.period.firstPhaseId, ctx.period.lastPhaseId)
      }
      val transformer = new TreeTransformer {
        override def miniPhases = Array(new Transformation1, new Transformation2)

        override def phaseName: String = "test"
      }
      val tr = transformer.macroTransform(tree).toString

      Assert.assertTrue("node can rewrite children",
        tr.contains("Literal(Constant(3))")
      )
  }

  @Test
  def invocationCount: Unit = checkCompile("frontend", "class A{ val d = 1}") {
    (tree, context) =>
      implicit val ctx = context
      var transformed1 = 0
      class Transformation1 extends MiniPhaseTransform {
        override def phaseName: String = "invocationCount1"
        override def transformLiteral(tree: tpd.Literal)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
          transformed1 += 1
          Assert.assertTrue("correct constant",
            tree.const.toString == "Constant(1)"
          )
          tpd.cpy.Literal(tree)(Constant(-1))
        }

        override def transformValDef(tree: tpd.ValDef)(implicit ctx: Context, info: TransformerInfo) = {
          transformed1 += 1
          Assert.assertTrue("transformation of children succeeded",
            tree.rhs.toString == "Literal(Constant(-3))"
          )
          tpd.cpy.ValDef(tree)(rhs = transformFollowing(tpd.Literal(Constant(2))))
        }

        init(ctx, ctx.period.firstPhaseId, ctx.period.lastPhaseId)
      }
      var transformed2 = 0
      class Transformation2 extends MiniPhaseTransform {
        var constantsSeen = 0
        override def phaseName: String = "invocationCount2"
        override def transformLiteral(tree: tpd.Literal)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
          transformed2 += 1
          constantsSeen match {
            case 0 =>
              Assert.assertTrue("correct constant",
                tree.const.toString == "Constant(-1)"
              )
            case 1 =>
              Assert.assertTrue("correct constant",
                tree.const.toString == "Constant(2)"
              )
            case _ => Assert.fail("to many constants seen")
          }
          constantsSeen += 1
          tpd.cpy.Literal(tree)(Constant(-3))
        }

        override def transformValDef(tree: tpd.ValDef)(implicit ctx: Context, info: TransformerInfo) = {
          transformed2 += 1
          Assert.assertTrue("transformation of children succeeded",
            tree.rhs.toString == "Literal(Constant(-3))"
          )
          transformFollowing(tpd.cpy.ValDef(tree)(rhs = tpd.Literal(Constant(3))))
        }

        init(ctx, ctx.period.firstPhaseId, ctx.period.lastPhaseId)
      }
      val transformer = new TreeTransformer {
        override def miniPhases = Array(new Transformation1, new Transformation2)

        override def phaseName: String = "test"
      }
      val tr = transformer.macroTransform(tree).toString
      Assert.assertTrue("transformations aren't invoked multiple times",
        transformed1 == 2 && transformed2 == 3
      )
  }
}
