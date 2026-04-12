package coll

trait MapOps[K]:
  this: MapOps[K]^ =>
  def keysIterator: Iterator[K]

  private trait GenKeySet[K]:
    this: collection.Set[K] =>
    private[MapOps] val allKeys = MapOps.this.keysIterator.toSet