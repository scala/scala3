import scala.compiletime.testing.*

// The `DivideZeroPlugin` phase `divideZeroCheck` runs after `pickler`, which is
// past the phases that the single-argument `typeCheckErrors`/`typeChecks` run.
// The two-argument overloads additionally run enabled plugin phases up to (and
// including) the named phase, so the plugin's diagnostic is observed.
@main def Test: Unit =
  // Without a stop-after phase, plugin phases are not run: the code type-checks.
  val withoutPhase = typeCheckErrors("5 / 0")
  assert(withoutPhase.isEmpty, s"expected no errors without a phase, got: $withoutPhase")
  assert(typeChecks("5 / 0"), "expected `typeChecks` to succeed without a phase")

  // Requesting the plugin phase surfaces the plugin's error.
  val withPhase = typeCheckErrors("5 / 0", "divideZeroCheck")
  assert(withPhase.nonEmpty, "expected the plugin phase to report an error")
  assert(
    withPhase.exists(e => e.message == "divide by zero" && e.kind == ErrorKind.Typer),
    s"expected a `divide by zero` error, got: ${withPhase.map(e => (e.message, e.kind))}"
  )
  assert(!typeChecks("5 / 0", "divideZeroCheck"), "expected `typeChecks` to fail once the plugin phase runs")

  // Ordinary type/parse errors are still reported by the two-argument overload,
  // and a genuinely fine snippet still type-checks with the plugin phase run.
  assert(typeCheckErrors("val x: Int = \"\"", "divideZeroCheck").nonEmpty, "expected a type error to be reported")
  assert(typeChecks("5 / 2", "divideZeroCheck"), "expected a valid snippet to type-check with the plugin phase")
