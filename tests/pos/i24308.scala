class ValHolder(val v: Int):
  self =>
  inline def doesntCompile: ValHolder { val v: self.v.type } = ???
end ValHolder
