package com.typesafe.tools.mima

import com.github.lolgab.mill.{mima => millmima}

/**
  * Helpers to compile MiMA-related code shared with the sbt build
  */
object core {
  val ProblemFilters = millmima.ProblemFilter
  type ProblemFilter = millmima.ProblemFilter

  type DirectMissingMethodProblem = millmima.DirectMissingMethodProblem
  type FinalClassProblem = millmima.FinalClassProblem
  type MissingTypesProblem = millmima.MissingTypesProblem
  type MissingClassProblem = millmima.MissingClassProblem
  type MissingFieldProblem = millmima.MissingFieldProblem
  type FinalMethodProblem = millmima.FinalMethodProblem
}
