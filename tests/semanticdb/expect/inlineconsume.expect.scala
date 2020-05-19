package inlineconsume

import inlinedefs.FakePredef/*->inlinedefs::FakePredef.*/.assert/*->inlinedefs::FakePredef.assert().*/

class Foo/*<-inlineconsume::Foo#*/:
  def test/*<-inlineconsume::Foo#test().*/ = assert/*->inlinedefs::FakePredef.assert().*/(3 >/*->scala::Int#`>`(+3).*/ 2)
