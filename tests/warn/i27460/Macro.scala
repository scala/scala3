//> using options -release 17

// The test harness passes `-Xtarget`, which `-release` overrides, reported once per run. // nopos-warn // nopos-warn

import scala.quoted.*

inline def id(inline x: Int): Int = ${ idImpl('x) }
def idImpl(x: Expr[Int])(using Quotes): Expr[Int] = x

// Under `-release`, `Integer`'s `@jdk.internal.ValueBased` annotation refers to a synthetic symbol
def lock1(i: Integer) = i.synchronized {} // warn
