package tests

object SpecializeUtils {

  def checkMethodExists(cls: Class[_], name: String, params: List[Class[_]], ret: Class[_], specialized: Boolean = true): Unit = {
    val nameMatch = if (specialized) name + "\\$spec\\d*" else name
    val methods = cls.getDeclaredMethods
    assert(methods.count(x => x.getName.matches(nameMatch) && x.getParameterTypes.toList == params && x.getReturnType == ret) == 1)
  }

}
