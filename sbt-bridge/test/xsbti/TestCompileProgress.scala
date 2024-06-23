package xsbti

import xsbti.compile.CompileProgress

import scala.collection.mutable

class TestCompileProgress extends CompileProgress:
  class Run:
    private[TestCompileProgress] val _phases: mutable.Set[String] = mutable.LinkedHashSet.empty
    private[TestCompileProgress] val _unitPhases: mutable.Map[String, mutable.Set[String]] = mutable.LinkedHashMap.empty
    private[TestCompileProgress] var _latestTotal: Int = 0

    def phases: List[String] = _phases.toList
    def unitPhases: collection.MapView[String, List[String]] = _unitPhases.view.mapValues(_.toList)
    def total: Int = _latestTotal

  private val _runs: mutable.ListBuffer[Run] = mutable.ListBuffer.empty
  private var _currentRun: Run = new Run

  def runs: List[Run] = _runs.toList

  def completeRun(): Unit =
    _runs += _currentRun
    _currentRun = new Run

  override def startUnit(phase: String, unitPath: String): Unit =
    _currentRun._unitPhases.getOrElseUpdate(unitPath, mutable.LinkedHashSet.empty) += phase

  override def advance(current: Int, total: Int, prevPhase: String, nextPhase: String): Boolean =
    _currentRun._phases += prevPhase
    _currentRun._phases += nextPhase
    _currentRun._latestTotal = total
    true
