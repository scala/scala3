object T {
  val ok = summon[ValueOf[Unit]]
  val ko = M.valueOfUnit
}
