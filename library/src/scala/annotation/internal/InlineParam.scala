package scala.annotation.internal

import language.experimental.captureChecking

import scala.annotation.Annotation

/** An annotation produced by Namer to indicate an inline parameter. */
final class InlineParam() extends Annotation
