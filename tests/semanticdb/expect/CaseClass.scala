package caseclass

case class CaseClass(int1: Int, int2: Int)

object CaseClass:
  def apply(int: Int): CaseClass = CaseClass(int, 0)
  def apply(): CaseClass = CaseClass(0, 0)
