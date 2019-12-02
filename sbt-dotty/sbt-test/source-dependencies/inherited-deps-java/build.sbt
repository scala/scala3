// this test is specific to the old incremental compilation algorithm
incOptions := incOptions.value.withNameHashing(false)

lazy val verifyDeps = taskKey[Unit]("verify inherited dependencies are properly extracted")

verifyDeps := {
	val a = compile.in(Compile).value
	same(a.relations.publicInherited.internal.forwardMap, expectedDeps.forwardMap)
}

lazy val expected = Seq(
	"A" -> Seq("C", "D", "E", "G", "J"),
	"B" -> Seq("C", "D", "G", "J"),
	"C" -> Seq("D", "G", "J"),
	"D" -> Seq("G", "J"),
	"E" -> Seq(),
	"F" -> Seq(),
	"G" -> Seq("J"),
	"J" -> Seq()
)
lazy val pairs =
	expected.map { case (from,tos) =>
		(toFile(from), tos.map(toFile))
	}
lazy val expectedDeps = pairs.foldLeft(Relation.empty[File,File]) { case (r, (x,ys)) => r + (x,ys) }
def toFile(s: String) = file(s + ".java").getAbsoluteFile

def same[T](x: T, y: T): Unit = {
	assert(x == y, s"\nActual: $x, \nExpected: $y")
}
