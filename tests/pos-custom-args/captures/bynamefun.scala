object test:
  class Plan(elem: Plan)
  object SomePlan extends Plan(???)
  def f1(expr: (-> Plan) -> Plan): Plan = expr(SomePlan)
  f1 { onf => Plan(onf) }
  def f2(expr: (=> Plan) -> Plan): Plan = ???
  f2 { onf => Plan(onf) }
  def f3(expr: (-> Plan) => Plan): Plan = ???
  f1 { onf => Plan(onf) }
  def f4(expr: (=> Plan) => Plan): Plan = ???
  f2 { onf => Plan(onf) }
