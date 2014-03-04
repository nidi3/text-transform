package guru.nidi.text.transform.format

import guru.nidi.text.transform.{Formatter, Segment}
import guru.nidi.text.transform.Name._
import guru.nidi.text.transform.Attribute._

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