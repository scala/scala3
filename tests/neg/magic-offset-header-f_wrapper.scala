//> using options -Ymagic-offset-header:TEST_MARKER -language:strictEquality
val t1 = 1
val t2 = 2
val t3 = 3
///TEST_MARKER:tests/neg/magic-offset-header-f.scala
import language.strictEquality

class Foo
class Bar

def test2 = new Foo == new Bar // anypos-error
