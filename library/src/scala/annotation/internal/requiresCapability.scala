package scala.annotation.internal
import annotation.StaticAnnotation

/** An annotation to record a required capaility in the type of a throws
 */
class requiresCapability(capability: Any) extends StaticAnnotation

