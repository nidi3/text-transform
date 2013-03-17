package stni.text.transform

/**
 *
 */
class Attribute private(name: String) extends Const(name) {
  def apply(index: Int) = new Attribute(name + index)

  def ->(value: Any) = new AttributePair(this, value)
}

object Attribute {
  val TEXT = Attribute("text")
  val LEVEL = Attribute("level")
  val TARGET = Attribute("target")
  val COLUMNS = Attribute("columns")
  val ROWS = Attribute("rows")
  val FLOAT = Attribute("float")
  val ORIGINAL = Attribute("original")
  val TYPE = Attribute("type")
  val SPAN = Attribute("span")
  val WIDTH = Attribute("width")
  val HEIGHT = Attribute("height")
  val ANGLE = Attribute("angle")
  val ALIGN = Attribute("align")
  val HEADER = Attribute("header")
  val CAPTION = Attribute("caption")

  def apply(name: String) = new Attribute(name)
}

