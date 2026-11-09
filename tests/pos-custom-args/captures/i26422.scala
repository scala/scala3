//> using options -language:experimental.modularity

import language.experimental.captureChecking
import language.experimental.modularity
import caps.*

// Like i26347, but the adopted root sits in a capture refinement of a tracked class parameter.

abstract class FS extends caps.SharedCapability:
  def root: Entry^{this}

abstract class Entry(tracked val origin: FS):
  self =>
  def name: String
  def children: List[Entry^{this}]

def test_fs =
  val fs: FS^ = ???
  val entries = fs.root.children
  val names = entries.map(e => e.name)
  println(names)
