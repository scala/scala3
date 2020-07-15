class FrameworkTest(val cls: Class[_], val format: Int = 1, val expected: String)

class FixtureFrameworkSuite
object FixtureFrameworkSuite extends FrameworkTest(
    classOf[FixtureFrameworkSuite],
    expected = "test"
)

@main
def Test = FixtureFrameworkSuite
