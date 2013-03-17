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
  def apply(values: PseudoSegment*) = Segment(this, values: _*)
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
class Segment(val name: Name) extends PseudoSegment {
  val attributes = Map[Attribute, Any]()
  val children = ListBuffer[Segment]()
  private var _parent: Option[Segment] = None

  def apply(name: Attribute): Option[Any] = attributes.get(name)

  def apply(values: PseudoSegment*) = add(values: _*)

  def parent = this._parent

  def addTo(parent: Segment) = {
    parent.add(this)
    this
  }

  def add(values: PseudoSegment*): Segment = {
    values.foreach(_ match {
      case seg: Segment =>
        seg._parent = Some(this)
        children += seg
      case attr: AttributePair =>
        attributes.put(attr._1, attr._2)
    })
    this
  }

  def addAttribute(name: Attribute, value: Any): Segment = add(new AttributePair(name, value))

  def addChild(child: Segment): Segment = add(child)

  def root: Segment = {
    parent match {
      case Some(seg) => seg.root
      case None => this
    }
  }

  override def toString = toFormattedString(0)

  private def toFormattedString(level: Int): String = {
    def indent = "  " * level
    def ch = {
      if (children.isEmpty) {
        ""
      } else {
        ", children=\n" +
          children.map(child => child.toFormattedString(level + 1)).mkString("\n") +
          indent
      }
    }
    indent + s"{$name, $attributes $ch}"
  }

  override def equals(other: Any) = {
    if (!other.isInstanceOf[Segment]) false
    else {
      val s = other.asInstanceOf[Segment]
      s.attributes == attributes && s.children == children
    }
  }

  override def hashCode = {
    (attributes.hashCode + children.hashCode * 31)
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
  val ITEM = new Name("item")
  val TABLE = new Name("table")
  val TABLE_CELL = new Name("tableCell")
  val LINE = new Name("line")
  val IMAGE = new Name("image")
  val SYMBOL = new Name("symbol")
  val NEWLINE = new Name("newline")
  val DEFINITION = new Name("definition")
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
  val FILE = new AttributeValue("file")
  val DOCUMENT = new AttributeValue("document")
  val URL = new AttributeValue("url")
}

object Segment {
  def apply(name: Name, children: PseudoSegment*): Segment = new Segment(name).add(children: _*)

  def plain(text: String): Segment = PLAIN(TEXT -> text)

  def symbol(original: String, typ: AttributeValue): Segment = SYMBOL(ORIGINAL -> original, TYPE -> typ)
}
