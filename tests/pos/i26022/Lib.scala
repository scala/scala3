// https://github.com/scala/scala3/issues/26022
object Lib:
  opaque type Mat = String
  opaque type CVec <: Mat = Mat

  extension (m: Mat)
    def tdata: Int = ???
    inline def apply(): Int = m.tdata

  def linspace: CVec = ???
