// https://github.com/scala/scala3/issues/25702
// Regression test: a concrete class missing trait methods must produce a clean
// diagnostic, not a compiler crash. The original crash needed a stale
// `scala3-library` where `scala.caps.internal.consume` was missing, which made
// `requiredClass` fall back to `defn.AnyClass`. We can't reproduce the
// stale-classpath setup in the standard test framework, but the underlying fix
// is exercised by the `RequiredClassTest` unit test; this file guards the
// "missing-method" diagnostic.

trait Storage:
  def resolve(address: String): String
  def create(data: List[Byte], owner: Option[String] = None): String

final class InMemoryStorage extends Storage // error
