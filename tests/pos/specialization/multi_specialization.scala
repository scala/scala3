object multi_specialization {
  def one[@specialized T](n: T): T = n
  def two[@specialized T, U](n: T, m: U): (T,U) = (n,m)
  def three[@specialized T, U, V](n: T, m: U, o: V): (T,U,V) = (n,m,o)

  one(1)
  two(1,2)
  two('a', null)
}