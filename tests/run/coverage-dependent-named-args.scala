trait Key:
  type Value
  def value: Value

def accept(key: Key, value: key.Value): Unit = ()

final class Probe:
  private var events = List.empty[String]

  private def same(key: Key, label: String): key.type =
    events :+= label
    key

  def check(key: Key): Unit =
    accept(
      value = same(key, "value").value,
      key = same(key, "key")
    )
    assert(events == List("value", "key"), events)

final class IntKey extends Key:
  type Value = Int
  val value = 1

@main def Test =
  new Probe().check(new IntKey)
