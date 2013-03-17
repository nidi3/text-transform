package org.mimacom.text.transform.format

import org.mimacom.text.transform.{Formatter, Segment}
import org.mimacom.text.transform.Name._
import org.mimacom.text.transform.Attribute._

class SimpleFormatter extends Formatter {
  def format(root: Segment) = {
    root.children.map(segment => {
      segment.name match {
        case PLAIN => segment(TEXT).get
        case SYMBOL => segment(ORIGINAL).get
        case _ => ""
      }
    }).mkString
  }
}