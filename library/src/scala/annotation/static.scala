/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package scala.annotation

import scala.annotation.meta._

/** https://github.com/scala/scala.github.com/pull/491 */

@field
@getter
@beanGetter
@beanSetter
@param
@setter
final class static extends StaticAnnotation
