package scala.annotation.internal
import annotation.StaticAnnotation

import language.experimental.captureChecking

/** An annotation to record a required capaility in the type of a throws */
class requiresCapability(capability: Any) extends StaticAnnotation

