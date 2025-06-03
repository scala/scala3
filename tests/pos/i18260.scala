import scala.deriving.Mirror

type Id[A] = A
object Id:
  def apply[A](a: A): Id[A] = a

def test[P <: Product](using mirror: Mirror.ProductOf[P])(tuples: Tuple.Map[mirror.MirroredElemTypes, Id]): Tuple.Map[mirror.MirroredElemTypes, Id] = tuples

def main() =
  case class Test(p1: 1, p2: 2, p3: 3, p4: 4, p5: 5, p6: 6, p7: 7, p8: 8, p9: 9, p10: 10, p11: 11, p12: 12, p13: 13, p14: 14, p15: 15, p16: 16, p17: 17, p18: 18, p19: 19, p20: 20, p21: 21, p22: 22, p23: 23)
  test[Test](Id[1](1), Id[2](2), Id[3](3), Id[4](4), Id[5](5), Id[6](6), Id[7](7), Id[8](8), Id[9](9), Id[10](10), Id[11](11), Id[12](12), Id[13](13), Id[14](14), Id[15](15), Id[16](16), Id[17](17), Id[18](18), Id[19](19), Id[20](20), Id[21](21), Id[22](22), Id[23](23))
