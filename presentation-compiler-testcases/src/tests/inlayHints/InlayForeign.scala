package tests.inlayHints

/** Fixture for [[dotty.tools.pc.tests.inlayHints.InlayHintsInlinedDependencySuite]];
 *  see that suite for the full explanation of the crash it reproduces.
 *
 *  Must stay a `transparent inline def` returning a constructor `Apply`: the
 *  `transparent inline` expansion keeps its trees positioned in *this* source,
 *  whereas a plain `inline def` would remap them to the call site and would not
 *  reproduce the crash.
 */
final class Into(val source: String)

transparent inline def into(source: String): Into =
  new Into(source)
