package guru.nidi.text.transform

/**
 *
 */
class Name private(name: String) extends Const(name) {
  def apply(values: PseudoSegment*) = Segment(this, values: _*)
}

object Name {
  val ROOT = new Name("root")
  val PLAIN = new Name("plain")
  val BOLD = new Name("bold")
  val ITALICS = new Name("italics")
  val UNDERLINED = new Name("underlined")
  val HEADING = new Name("heading")
  val LINK = new Name("link")
  val LIST = new Name("list")
  val ITEM = new Name("item")
  val TABLE = new Name("table")
  val TABLE_CELL = new Name("tableCell")
  val LINE = new Name("line")
  val IMAGE = new Name("image")
  val SYMBOL = new Name("symbol")
  val NEWLINE = new Name("newline")
  val DEFINITION = new Name("definition")
}
