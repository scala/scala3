/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package dotty.tools
package dotc
package interactive

import core._
import Phases._
import typer._

class InteractiveCompiler extends Compiler {
  // TODO: Figure out what phases should be run in IDEs
  // More phases increase latency but allow us to report more errors.
  // This could be improved by reporting errors back to the IDE
  // after each phase group instead of waiting for the pipeline to finish.
  override def phases: List[List[Phase]] = List(
    List(new FrontEnd),
    List(new transform.SetRootTree),
    List(new transform.CookComments)
  )
}
