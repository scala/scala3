package dotty.tools
package dotc
package reporting

/**
 * This class mixes in a few standard traits, so that it is easier to extend from Java.
 */
abstract class AbstractReporter extends Reporter with UniqueMessagePositions with HideNonSensicalMessages with MessageRendering
