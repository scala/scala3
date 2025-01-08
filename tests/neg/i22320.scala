package a:
  opaque type System = Any
  opaque type SimulatedSystem <: System = System

  extension (system: System)
    def x: BigInt = ???
    def y: BigInt = ???
  end extension

  extension (system: SimulatedSystem)
    def z: BigInt = ???
  end extension

package b:
  import a.*
  def issue(system: System) =
    val x = system.x
    val y = system.y
    val z = system.z // error