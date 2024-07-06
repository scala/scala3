object Outer:
  def Test =
    object Inner:
      var x: Int = 2
      class Rgb():
        def f = x

    type Id[X] = X
    type TRgb = Id[Inner.Rgb]

    val ok = new Inner.Rgb()
    val crash = new Id[Inner.Rgb]