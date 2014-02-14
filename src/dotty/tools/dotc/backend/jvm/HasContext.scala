/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package dotty.tools.dotc
package backend.jvm

import core.Contexts.Context

/**
 * All components (e.g. BCPickles, BCInnerClassGen) of the builder classes
 * extend this trait to have access to the context.
 *
 * The context is provided by the three leaf classes (PlainClassBuilder,
 * JMirrorBuilder and JBeanInfoBuilder) as class parameter.
 * 
 * Same goes for BytecodeWriter
 */
trait HasContext {
  implicit protected val ctx: Context
}


