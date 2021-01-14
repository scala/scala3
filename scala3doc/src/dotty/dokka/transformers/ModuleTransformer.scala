package dotty.dokka

import org.jetbrains.dokka.model._

abstract trait ModuleTransformer:
  def apply(m: DModule): DModule