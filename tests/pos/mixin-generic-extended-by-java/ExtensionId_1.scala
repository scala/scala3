trait Extension

class ClassicActorSystemProvider

/**
 * Identifies an Extension
 * Lookup of Extensions is done by object identity, so the Id must be the same wherever it's used,
 * otherwise you'll get the same extension loaded multiple times.
 */
trait ExtensionId[T <: Extension] {

  def get(system: ClassicActorSystemProvider): T = ???
}

/**
 * Java API for ExtensionId
 */
abstract class AbstractExtensionId[T <: Extension] extends ExtensionId[T]
