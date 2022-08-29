package scala.annotation.internal

import annotation.{StaticAnnotation, experimental}

/** An annotation to record a required capaility in the type of a throws
 */
@experimental class requiresCapability(capability: Any) extends StaticAnnotation

