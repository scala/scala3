abstract class Quantity[A <: Quantity[A]]
class Energy extends Quantity[Energy]
class Time extends Quantity[Time]
class Dimensionless extends Quantity[Dimensionless]

class Price[Q <: Quantity[Q] & (Energy | Time | Dimensionless)]