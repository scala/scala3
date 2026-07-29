package dotty.tools.scaladoc

import org.junit.Assert.assertEquals

class PackageOrderingTest extends ScaladocTest(""):
  override def moduleDocContext =
    testDocContext(
      tastyFiles("", rootPck = "packageorderingalpha") ++
        tastyFiles("", rootPck = "packageorderingbeta")
    )

  override def runTest: Unit = withModule { module =>
    val packages = module.rootPackage.members
      .filter(_.name.startsWith("packageordering"))
      .map(_.name)
    assertEquals(List("packageorderingalpha", "packageorderingbeta"), packages)

    val subpackages = module.rootPackage.members
      .find(_.name == "packageorderingalpha")
      .toList
      .flatMap(_.members)
      .filter(_.name.startsWith("packageorderingalpha."))
      .map(_.name)
    assertEquals(
      List("packageorderingalpha.aardvark", "packageorderingalpha.zebra"),
      subpackages
    )
  }
