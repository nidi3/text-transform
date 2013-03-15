package org.mimacom.text.transform.parse.html.confluence

import org.mimacom.text.transform.parse.html.HtmlParser
import xml.Node
import org.mimacom.text.transform.Segment
import org.mimacom.text.transform.Name._
import org.mimacom.text.transform.Attribute._

/**
 *
 */
class ConfluenceHtmlParser extends HtmlParser {
  override def namespaces = List("ac","ri")

  override def parse(node: Node, listLevel: Int): Seq[Segment] = {
    node match {
      case n @ <ac:link>{ns @ _*}</ac:link> =>
        val filename = n \ "attachment" \ "@{ri}filename"
        if (filename.isEmpty) Nil else {
            List(Segment(LINK,TARGET->filename(0).text,Segment.plainText(filename(0).text)))
        }
      case _ => super.parse(node, listLevel)
    }
  }
}

