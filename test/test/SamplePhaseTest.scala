package test

import org.junit.{Assert, Test}

class SamplePhaseTest extends DottyTest {

  // Disabled, awaiting resolution: @Test
  def testTypechekingSimpleClass = checkCompile("frontend", "class A{}") {
    (tree, context) =>
      implicit val ctx = context
      Assert.assertTrue("can typecheck simple class",
        tree.toString == "PackageDef(Ident(<empty>),List(TypeDef(Modifiers(,,List()),A,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree[TypeRef(ThisType(module class scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class lang),Object)]),<init>),List())),ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),List()))))"
      )
  }

}
