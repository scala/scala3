abstract class Quantity[A <: Quantity[A]]
sealed trait UnitOfMeasure[A <: Quantity[A]]

class Time extends Quantity[Time]
object Minutes extends UnitOfMeasure[Time]

class PowerRamp extends Quantity[PowerRamp]
object KilowattsPerHour extends UnitOfMeasure[PowerRamp]

type Test[X <: UnitOfMeasure[?]] = X match
  case UnitOfMeasure[t] => t

@main def main =
  summon[Test[Minutes.type] =:= Time]
  summon[Test[KilowattsPerHour.type] =:= PowerRamp]
