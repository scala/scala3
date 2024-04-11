ThisBuild / usePipelining := true

// defines a macro, sbt will not force the early output
// because it will detect macros in the analysis, so b will compile fine,
// see `sbt-test/pipelining/pipelining-scala-macro-fail` for how we can
// force a failure by always forcing early output.
lazy val a = project.in(file("a"))
  .settings(
    // scalacOptions += "-Ycheck:all",
    scalacOptions += "-Xprint-suspension",
    Compile / incOptions := {
      val old = (Compile / incOptions).value
      val hooks = old.externalHooks
      val newHooks = hooks.withExternalLookup(
        new sbt.internal.inc.NoopExternalLookup {
          @volatile var earlyOutputChecks = 0

          def didFindMacros(analysis: xsbti.compile.CompileAnalysis) = {
            val foundMacros = analysis.asInstanceOf[sbt.internal.inc.Analysis].apis.internal.values.exists(_.hasMacro)
            assert(foundMacros, "expected macros to be found in the analysis.")
            foundMacros
          }

          // force early output, this is safe because the macro class from `macros` will be available.
          override def shouldDoEarlyOutput(analysis: xsbti.compile.CompileAnalysis): Boolean = {
            earlyOutputChecks += 1
            assert(earlyOutputChecks <= 2, "should only be called twice (apiPhaseCompleted, dependencyPhaseCompleted).")
            val internalClasses = analysis.asInstanceOf[sbt.internal.inc.Analysis].apis.internal
            val a_A = internalClasses.get("a.A")
            val a_ASuspendTyper = internalClasses.get("a.ASuspendTyper")
            val a_ASuspendInlining = internalClasses.get("a.ASuspendInlining")

            // both `a.A` and `a.ASuspendInlining` should be found in the analysis.
            // even though `a.ASuspendInlining` suspends, it happens at inlining, so we should still
            // record API for it in the first run.
            assert(a_A.isDefined, s"`a.A` wasn't found.")
            assert(a_ASuspendInlining.isDefined, s"`a.ASuspendInlining` wasn't found.")

            // in run 1, `a.ASuspendTyper` would have suspended at typer, and not be present in Analysis.
            // Therefore we wouldn't close the early output jar.
            // Therefore, because it is present here, we waited to the second run to close the early output jar,
            // at which point we recorded API for `a.ASuspendTyper`, and because we closed the early output jar,
            // we send the signal to Zinc that the early output was written.
            assert(a_ASuspendTyper.isDefined, s"`a.ASuspendTyper` wasn't found.")


            // do what sbt does typically,
            // it will not force early output because macros are found
            !didFindMacros(analysis)
          }
        }
      )
      old.withExternalHooks(newHooks)
    },
  )

// uses the macro, sbt is smart enough to not use pipelining flags when upstream compilation has macros
lazy val b = project.in(file("b"))
  .dependsOn(a)
  .settings(
    scalacOptions += "-Ycheck:all",
  )
