object Test { self => // error: objects must not have a self type
  def a = 5
  self.a // error: not found: self
}
