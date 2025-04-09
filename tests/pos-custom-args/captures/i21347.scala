//!> using scala 3.6.0-RC1-bin-SNAPSHOT

import language.experimental.captureChecking

class Box[cap Cap] {}

def run[cap Cap](f: Box[{Cap}]^{Cap} => Unit): Box[{Cap}]^{Cap} = ???

def main() =
  val b = run(_ => ())
  // val b = run[caps.CapSet](_ => ()) // this compiles
