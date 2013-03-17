package org.mimacom.text.transform

/**
 *
 */
class AttributeValue private(name: String) extends Const(name)

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