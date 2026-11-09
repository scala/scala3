package mytest

import repro.Crash

// Should not crash with AssertionError: start of NoSpan.
// typeCheckErrors is inline -> InlineTyper (ReTyper) is active.
// TypeMismatch lazy message -> importSuggestions.deepTest ->
// typedImplicit -> adaptToArgs fallBack -> union.startLine on NoSpan.
@main def Test() =
  val errors = scala.compiletime.testing.typeCheckErrors("repro.Crash.trigger")
  println(s"errors: ${errors.length}")
