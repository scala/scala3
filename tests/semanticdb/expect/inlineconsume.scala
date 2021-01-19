package inlineconsume

import inlinedefs.FakePredef.assert

class Foo with
  def test = assert(3 > 2)
