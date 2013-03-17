package stni.text.transform.format

import stni.text.transform.{Formatter, Segment}
import stni.text.transform.Name._
import stni.text.transform.Attribute._

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