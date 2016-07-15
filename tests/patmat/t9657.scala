sealed trait PowerSource

case object Petrol extends PowerSource

case object Pedal extends PowerSource

sealed abstract class Vehicle {
  type A <: PowerSource
}

case object Bicycle extends Vehicle {
  type A = Pedal.type
}

case class Bus(fuel: Int) extends Vehicle {
  type A = Petrol.type
}

case class Car(fuel: Int) extends Vehicle {
  type A = Petrol.type
}

class Test {
  def refuel[P <: Petrol.type](vehicle: Vehicle {type A = P} ): Vehicle = vehicle match {
    case Car(_) => Car(100)
    case Bus(_) => Bus(100)
  }

  def refuel2[P <: Petrol.type](vehicle: Vehicle {type A = P} ): Vehicle = vehicle match {
    case Car(_) => Car(100)
  }

  def foo1(vehicle: Vehicle {type A <: Petrol.type} ): Vehicle = vehicle match {
    case Car(_) => Car(100)
    case Bus(_) => Bus(100)
  }

  def foo2(vehicle: Vehicle {type A <: Petrol.type} ): Vehicle = vehicle match {
    case Car(_) => Car(100)
  }

  type P = Petrol.type

  def bar1(vehicle: Vehicle {type A <: P} ): Vehicle = vehicle match {
    case Car(_) => Car(100)
    case Bus(_) => Bus(100)
  }

  def bar2(vehicle: Vehicle {type A <: P} ): Vehicle = vehicle match {
    case Car(_) => Car(100)
  }

  def qux1[P <: Petrol.type](vehicle: Vehicle {type A <: P} ): Vehicle = vehicle match {
    case Car(_) => Car(100)
    case Bus(_) => Bus(100)
  }

  def qux2[P <: Petrol.type](vehicle: Vehicle {type A <: P} ): Vehicle = vehicle match {
    case Car(_) => Car(100)
  }

}
