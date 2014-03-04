package guru.nidi.text.transform

/**
 *
 */
class Const(val name: String) {
  override def equals(obj: Any): Boolean = obj != null && obj.getClass == getClass && obj.asInstanceOf[Const].name == name

  override def hashCode = name.hashCode

  override def toString = name
}
