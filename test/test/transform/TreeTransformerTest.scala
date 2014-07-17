package test.transform


import org.junit.{Assert, Test}
import test.DottyTest
import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, TreeTransform, TreeTransformer}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context


class TreeTransformerTest extends DottyTest {

  @Test
  def shouldReturnSameTreeIfUnchanged = checkCompile("frontend", "class A{ val d = 1}") {
    (tree, context) =>
      implicit val ctx = context
      class EmptyTransform extends TreeTransform {
        override def name: String = "empty"
        init(ctx, ctx.period.firstPhaseId, ctx.period.lastPhaseId)
      }
      val transformer = new TreeTransformer {
        override def transformations = Array(new EmptyTransform)

        override def name: String = "test"
      }
      val transformed = transformer.transform(tree)

      Assert.assertTrue("returns same tree if unmodified",
        tree eq transformed
      )
  }

  @Test
  def canReplaceConstant = checkCompile("frontend", "class A{ val d = 1}") {
    (tree, context) =>
      implicit val ctx = context
      class ConstantTransform extends TreeTransform {

        override def transformLiteral(tree: tpd.Literal)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = tpd.Literal(Constant(2))
        override def name: String = "canReplaceConstant"
        init(ctx, ctx.period.firstPhaseId, ctx.period.lastPhaseId)
      }
      val transformer = new TreeTransformer {
        override def transformations = Array(new ConstantTransform)

        override def name: String = "test"
      }
      val transformed = transformer.transform(tree)

      Assert.assertTrue("returns same tree if unmodified",
        transformed.toString.contains("List(ValDef(Modifiers(,,List()),d,TypeTree[TypeRef(ThisType(module class scala),Int)],Literal(Constant(2)))")
      )
  }

  @Test
  def canOverwrite = checkCompile("frontend", "class A{ val d = 1}") {
    (tree, context) =>
      implicit val ctx = context
      class Transformation extends TreeTransform {

        override def transformLiteral(tree: tpd.Literal)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = tpd.Literal(Constant(-1))
        override def name: String = "canOverwrite"

        override def transformValDef(tree: tpd.ValDef)(implicit ctx: Context, info: TransformerInfo): tpd.ValDef = {
          Assert.assertTrue("transformation of children succeeded",
            tree.rhs.toString == "Literal(Constant(-1))"
          )
          tpd.cpy.ValDef(tree, tree.mods, tree.name, tree.tpt, tpd.Literal(Constant(2)))
        }

        init(ctx, ctx.period.firstPhaseId, ctx.period.lastPhaseId)
      }
      val transformer = new TreeTransformer {
        override def transformations = Array(new Transformation)

        override def name: String = "test"

      }
      val tr = transformer.transform(tree).toString

      Assert.assertTrue("node can rewrite children",
        tr.contains("Literal(Constant(2))") && !tr.contains("Literal(Constant(-1))")
      )
  }

  @Test
  def transformationOrder = checkCompile("frontend", "class A{ val d = 1}") {
    (tree, context) =>
      implicit val ctx = context
      class Transformation1 extends TreeTransform {
        override def name: String = "transformationOrder1"

        override def transformLiteral(tree: tpd.Literal)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
          Assert.assertTrue("correct constant",
            tree.const.toString == "Constant(1)"
          )
          tpd.cpy.Literal(tree, Constant(-1))
        }

        override def transformValDef(tree: tpd.ValDef)(implicit ctx: Context, info: TransformerInfo): tpd.ValDef = {
          Assert.assertTrue("transformation of children succeeded",
            tree.rhs.toString == "Literal(Constant(-1))"
          )
          tpd.cpy.ValDef(tree, tree.mods, tree.name, tree.tpt, tpd.Literal(Constant(2)))
        }

        init(ctx, ctx.period.firstPhaseId, ctx.period.lastPhaseId)
      }
      class Transformation2 extends TreeTransform {
        override def name: String = "transformationOrder2"
        override def transformValDef(tree: tpd.ValDef)(implicit ctx: Context, info: TransformerInfo): tpd.ValDef = {
          Assert.assertTrue("transformation of children succeeded",
            tree.rhs.toString == "Literal(Constant(2))"
          )
          tpd.cpy.ValDef(tree, tree.mods, tree.name, tree.tpt, tpd.Literal(Constant(3)))
        }

        init(ctx, ctx.period.firstPhaseId, ctx.period.lastPhaseId)
      }
      val transformer = new TreeTransformer {
        override def transformations = Array(new Transformation1, new Transformation2)

        override def name: String = "test"
      }
      val tr = transformer.transform(tree).toString

      Assert.assertTrue("node can rewrite children",
        tr.contains("Literal(Constant(3))")
      )
  }

  @Test
  def invocationCount = checkCompile("frontend", "class A{ val d = 1}") {
    (tree, context) =>
      implicit val ctx = context
      var transformed1 = 0
      class Transformation1 extends TreeTransform {
        override def name: String = "invocationCount1"
        override def transformLiteral(tree: tpd.Literal)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
          transformed1 += 1
          Assert.assertTrue("correct constant",
            tree.const.toString == "Constant(1)"
          )
          tpd.cpy.Literal(tree, Constant(-1))
        }

        override def transformValDef(tree: tpd.ValDef)(implicit ctx: Context, info: TransformerInfo) = {
          transformed1 += 1
          Assert.assertTrue("transformation of children succeeded",
            tree.rhs.toString == "Literal(Constant(-3))"
          )
          tpd.cpy.ValDef(tree, tree.mods, tree.name, tree.tpt, transformFollowing(tpd.Literal(Constant(2))))
        }

        init(ctx, ctx.period.firstPhaseId, ctx.period.lastPhaseId)
      }
      var transformed2 = 0
      class Transformation2 extends TreeTransform {
        var constantsSeen = 0
        override def name: String = "invocationCount2"
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
          tpd.cpy.Literal(tree, Constant(-3))
        }

        override def transformValDef(tree: tpd.ValDef)(implicit ctx: Context, info: TransformerInfo) = {
          transformed2 += 1
          Assert.assertTrue("transformation of children succeeded",
            tree.rhs.toString == "Literal(Constant(-3))"
          )
          transformFollowing(tpd.cpy.ValDef(tree, tree.mods, tree.name, tree.tpt, tpd.Literal(Constant(3))))
        }

        init(ctx, ctx.period.firstPhaseId, ctx.period.lastPhaseId)
      }
      val transformer = new TreeTransformer {
        override def transformations = Array(new Transformation1, new Transformation2)

        override def name: String = "test"
      }
      val tr = transformer.transform(tree).toString
      Assert.assertTrue("transformations aren't invoked multiple times",
        transformed1 == 2 && transformed2 == 3
      )
  }
}
