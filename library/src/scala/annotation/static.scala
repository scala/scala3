package scala.annotation

import language.experimental.captureChecking

import scala.annotation.meta.*

/** https://github.com/scala/scala.github.com/pull/491 */

@field
@getter
@beanGetter
@beanSetter
@param
@setter
final class static extends StaticAnnotation
