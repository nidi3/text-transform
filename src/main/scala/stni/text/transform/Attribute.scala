package stni.text.transform

/**
 *
 */
class Attribute[T] private(name: String) extends Const(name) {
  def apply(index: Int) = new Attribute[T](name + index)

  def ->(value: T) = new AttributePair(this, value)
}

object Attribute {
  val TEXT = Attribute[String]("text")
  val LEVEL = Attribute[Int]("level")
  val TARGET = Attribute[String]("target")
  val COLUMNS = Attribute[Int]("columns")
  val ROWS = Attribute[Int]("rows")
  val FLOAT = Attribute[Boolean]("float")
  val ORIGINAL = Attribute[String]("original")
  val TYPE = Attribute[AttributeValue]("type")
  val SPAN = Attribute[Int]("span")
  val WIDTH = Attribute[String]("width")
  val HEIGHT = Attribute[String]("height")
  val ANGLE = Attribute[String]("angle")
  val ALIGN = Attribute[AttributeValue]("align")
  val ALIGN_ALL = Attribute("align-all")
  val HEADER = Attribute[Boolean]("header")
  val CAPTION = Attribute[Segment]("caption")
  val SUB = Attribute[Segment]("sub")

  def apply[T](name: String) = new Attribute[T](name)
}

