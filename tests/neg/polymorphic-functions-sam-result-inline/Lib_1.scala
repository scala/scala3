trait WithLazy:
  def apply(x: Int): Int

trait WithStatic:
  def apply(x: Int): Int
  def get: Int = 0

inline def mk1: [A] => WithLazy = [A] => (x: Int) => x
inline def mk2: [A] => WithStatic = [A] => (x: Int) => x
