package org.mimacom.text.transform.parse.html.confluence

import org.mimacom.text.transform.Name._
import org.scalatest.FlatSpec
import org.mimacom.text.transform.{Segment, Name}
import scala.Predef._
import org.mimacom.text.transform.Attribute._


/**
 *
 */
class ConfluenceHtmlParserTest extends FlatSpec {
  val parser = new ConfluenceHtmlParser

  def parse(input: String) = parser.parse(input).children

  def assertSegmentEquals(name: Name, text: String, list: Seq[Segment], index: Int, subIndices: Int*) {
    var segment = list(index)
    subIndices.foreach(subIndex => segment = segment.children(subIndex))
    assertSegmentEquals(name, text, segment)
  }

  def assertSegmentEquals(name: Name, text: String, segment: Segment) {
    assert(name === segment.name)
    assert(text === segment(TEXT).getOrElse(null))
  }

  behavior of "<ac:link>"

  it should "unersand file attachments" in {
    val list = parse( """<ac:link><ri:attachment ri:filename="file" /></ac:link>""")
    assert(1 === list.size)
    assertSegmentEquals(LINK, null, list, 0)
    assert(list(0).attributes(TARGET) === "file")
    assertSegmentEquals(PLAIN, "file", list, 0, 0)
  }
}
