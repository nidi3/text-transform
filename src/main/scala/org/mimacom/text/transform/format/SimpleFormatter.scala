package org.mimacom.text.transform.format

import org.mimacom.text.transform.{Formatter, Segment}
import org.mimacom.text.transform.Name._
import org.mimacom.text.transform.Attribute._

class SimpleFormatter extends Formatter {
  def format(root: Segment) = {
    val result = new StringBuilder()
    root.children.foreach(segment => {
      segment.name match {
        case PLAIN => result.append(segment(TEXT).get)
        case SYMBOL => result.append(segment(ORIGINAL).get)
        case _ =>
      }
    })
    result.toString()
  }
}