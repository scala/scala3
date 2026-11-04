//>using options -language:experimental.inlineTraits
package test 

inline trait B:
  final def mkString: String = "B"