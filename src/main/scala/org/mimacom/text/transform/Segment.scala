package org.mimacom.text.transform

import collection.mutable.Map
import collection.mutable.ListBuffer
import Attribute._
import Name._

trait PseudoSegment

class Const(val name: String) {
  override def equals(obj: Any): Boolean = obj != null && obj.getClass == getClass && obj.asInstanceOf[Const].name == name

  override def hashCode = name.hashCode

  override def toString = name
}

class Name private(name: String) extends Const(name) {
}

class Attribute private(name: String) extends Const(name) {
  def index(index: Int) = new Attribute(name + index)

  def ->(value: Any) = new AttributePair(this, value)
}

class AttributePair(name: Attribute, value: Any) extends Pair(name, value) with PseudoSegment

class AttributeValue private(name: String) extends Const(name)


/**
 * A Segment is an abstract representation of a text. It is produced by a {@link org.mimacom.text.transform.Parser}
 * and consumed by a {@link Formatter}.
 * A Segment is defined by its name, a map of attributes and a list of child segments.
 */
class Segment private(val name: Name) extends PseudoSegment {
  val attributes = Map[Attribute, Any]()
  val children = ListBuffer[Segment]()
  private var _parent: Segment = null

  def apply(name: Attribute): Option[Any] = attributes.get(name)

  def parent = this._parent

  def addTo(parent: Segment) = {
    parent.add(this)
    this
  }

  def add(values: PseudoSegment*) = {
    values.foreach(_ match {
      case seg: Segment =>
        seg._parent = this
        children += seg
      case attr: AttributePair =>
        attributes.put(attr._1, attr._2)
    })
    this
  }

  //
  //
  //  public Segment getRoot() {
  //    Segment root = this;
  //    while (root.getParent() != null) {
  //      root = root.getParent();
  //    }
  //    return root;
  //  }
  //
  override def toString = {
    val s = new StringBuilder
    toFormattedString(s, 0)
    s.toString()
  }

  private def toFormattedString(s: StringBuilder, level: Int) {
    s.append("  " * level)
      .append("{")
      .append(name)
      .append(", ")
      .append(attributes)
    if (children.isEmpty) {
      s.append("}")
    } else {
      s.append(", children=\n")
      children.foreach(child => {
        child.toFormattedString(s, level + 1)
        s.append("\n")
      })
      s.append("  " * level).append("}")
    }
  }
}

object Name {
  val ROOT = new Name("root")
  val PLAIN = new Name("plain")
  val BOLD = new Name("bold")
  val ITALICS = new Name("italics")
  val HEADING = new Name("heading")
  val LINK = new Name("link")
  val LIST = new Name("list")
  val LIST_ITEM = new Name("listItem")
  val TABLE = new Name("table")
  val TABLE_CELL = new Name("tableCell")
  val LINE = new Name("line")
  val IMAGE = new Name("image")
  val SYMBOL = new Name("symbol")
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

object AttributeValue {
  val ORDERED = new AttributeValue("ordered")
  val UNORDERED = new AttributeValue("unordered")
  val ARROW_LEFT = new AttributeValue("larr")
  val ARROW_RIGHT = new AttributeValue("rarr")
  val ARROW_BOTH = new AttributeValue("barr")
  val DOUBLE_ARROW_LEFT = new AttributeValue("dlarr")
  val DOUBLE_ARROW_RIGHT = new AttributeValue("drarr")
  val DOUBLE_ARROW_BOTH = new AttributeValue("dbarr")
  val LEFT = new AttributeValue("left")
  val RIGHT = new AttributeValue("right")
}

object Segment {
  def apply(name: Name, children: PseudoSegment*): Segment = new Segment(name).add(children: _*)

  def plainText(text: String): Segment = Segment(PLAIN, TEXT -> text)

  def symbol(original: String, typ: AttributeValue): Segment = Segment(SYMBOL, ORIGINAL -> original, TYPE -> typ)
}
