trait MapOps[K]:
  this: MapOps[K]^ =>
  def keysIterator: Iterator[K]

  trait GenKeySet[K]:
    this: collection.Set[K] =>
    private[MapOps] val allKeys = MapOps.this.keysIterator.toSet