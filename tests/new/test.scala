import caps.unsafe.untrackedCaptures

class ArrayBuffer[A] private (@untrackedCaptures initElems: Array[AnyRef]^, initLength: Int):
  private var elems: Array[AnyRef]^ = initElems
  private var start = 0
  private var end = initLength

  def +=(elem: A): this.type =
    elems(end) = elem.asInstanceOf[AnyRef]
    this

  override def toString =
    val slice = elems.slice(start, end)
    val wrapped = wrapRefArray(slice)
    //val wrapped2 = collection.mutable.ArraySeq.ofRef(slice)
    val str = wrapped.mkString(", ")
    str
    //s"ArrayBuffer(${elems.slice(start, end).mkString(", ")})"
