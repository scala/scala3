//> using options -Wunused:all

trait Memberly:
  def member: Int

object Members:
  type MemberType <: Memberly
  type Empty

object Test:
  import Members.*

  type MT = MemberType
  def membered(using MT) = println() // warn abstract type offers member in upper bound
  def remembered(using mt: MT) = mt.member

  type Ignore = Empty
  def emptily(using Ignore) = println()
