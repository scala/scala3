trait SAMTrait:
  def first(): String
  def equals(obj: Int): Boolean

val m : SAMTrait = () => "Hello" // error
