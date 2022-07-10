class /[D, T]
class Delegating[D]

type Aux[E] = Container { type Elements = E }

class Container:
  type Elements = Delegating[Delegates]
  type Delegates

class Resolution[E](value: Aux[E]):
  type Type = Aux[E]

def element22(
    transmittable0: Resolution[?], transmittable1: Resolution[?],
    transmittable2: Resolution[?], transmittable3: Resolution[?],
    transmittable4: Resolution[?], transmittable5: Resolution[?],
    transmittable6: Resolution[?], transmittable7: Resolution[?],
    transmittable8: Resolution[?], transmittable9: Resolution[?],
    transmittable10: Resolution[?], transmittable11: Resolution[?],
    transmittable12: Resolution[?], transmittable13: Resolution[?],
    transmittable14: Resolution[?], transmittable15: Resolution[?],
    transmittable16: Resolution[?], transmittable17: Resolution[?],
    transmittable18: Resolution[?], transmittable19: Resolution[?],
    transmittable20: Resolution[?], transmittable21: Resolution[?])
: Container {
    type Delegates =
      transmittable0.Type / transmittable1.Type /
      transmittable2.Type / transmittable3.Type /
      transmittable4.Type / transmittable5.Type /
      transmittable6.Type / transmittable7.Type /
      transmittable8.Type / transmittable9.Type /
      transmittable10.Type / transmittable11.Type /
      transmittable12.Type / transmittable13.Type /
      transmittable14.Type / transmittable15.Type /
      transmittable16.Type / transmittable17.Type /
      transmittable18.Type / transmittable19.Type /
      transmittable20.Type / transmittable21.Type
  } = ???

def test22 =
  Resolution(
    element22(
      Resolution(element0), Resolution(element0), // error // error
      Resolution(element0), Resolution(element0), // error // error
      Resolution(element0), Resolution(element0), // error // error
      Resolution(element0), Resolution(element0), // error // error
      Resolution(element0), Resolution(element0), // error // error
      Resolution(element0), Resolution(element0), // error // error
      Resolution(element0), Resolution(element0), // error // error
      Resolution(element0), Resolution(element0), // error // error
      Resolution(element0), Resolution(element0), // error // error
      Resolution(element0), Resolution(element0), // error // error
      Resolution(element0), Resolution(element0)))// error // error
