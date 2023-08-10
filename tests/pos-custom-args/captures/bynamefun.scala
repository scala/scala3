object test:
  class Plan(elem: Plan)
  object SomePlan extends Plan(???)
  type PP = (-> Plan) -> Plan
  def f1(expr: (-> Plan) -> Plan): Plan = expr(SomePlan)
  f1 { onf => Plan(onf) }
  def f2(expr: (=> Plan) -> Plan): Plan = ???
  f2 { onf => Plan(onf) }
  def f3(expr: (-> Plan) => Plan): Plan = ???
  f3 { onf => Plan(onf) }
  def f4(expr: (=> Plan) => Plan): Plan = ???
  f4 { onf => Plan(onf) }
  def f5(expr: PP): Plan = expr(SomePlan)
  f5 { onf => Plan(onf) }