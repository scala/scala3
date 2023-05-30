package xsbt

import xsbti.*

class TestReporter extends Reporter:
  private val allProblems = collection.mutable.ListBuffer.empty[Problem]
  def comment(position: Position, msg: String): Unit = ()
  def hasErrors(): Boolean = allProblems.exists(_.severity == Severity.Error)
  def hasWarnings(): Boolean = allProblems.exists(_.severity == Severity.Warn)
  def log(problem: Problem): Unit = allProblems.append(problem)
  def printSummary(): Unit = ()
  def problems(): Array[Problem] = allProblems.toArray
  def reset(): Unit = allProblems.clear()
