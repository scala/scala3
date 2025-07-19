class Test:
  val _: this.type = summon[ValueOf[this.type]].value
