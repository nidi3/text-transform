package org.mimacom.text.transform.parse.html

import org.mimacom.text.transform.Name._
import org.scalatest.FlatSpec
import org.mimacom.text.transform.{Attribute, Segment, Name}
import scala.Predef._
import org.mimacom.text.transform.Attribute._
import org.mimacom.text.transform.AttributeValue._


/**
 *
 */
class HtmlParserTest extends FlatSpec {
  val parser = new HtmlParser

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

  "simple strings" should "be untouched" in {

    assert(0 === parse("").size)

    val list = parse("hey ho")
    assert(1 === list.size)
    assertSegmentEquals(PLAIN, "hey ho", list, 0)
  }

  behavior of "<p>"

  it should "add an empty line" in {
    val list = parse("hey <p>fat</p> ho")
    assert(4 === list.size)
    assertSegmentEquals(PLAIN, "hey ", list, 0)
    assertSegmentEquals(PLAIN, "fat", list, 1)
    assertSegmentEquals(PLAIN, "\r\n\r\n", list, 2)
    assertSegmentEquals(PLAIN, " ho", list, 3)
  }

  behavior of "<span>"

  it should "be ignored" in {
    val list = parse("hey <span>fat</span> ho")
    assert(3 === list.size)
    assertSegmentEquals(PLAIN, "hey ", list, 0)
    assertSegmentEquals(PLAIN, "fat", list, 1)
    assertSegmentEquals(PLAIN, " ho", list, 2)
  }

  behavior of "<br>"

  it should "add an empty line" in {
    val list = parse("hey <br/><br /><br></br> ho")
    assert(5 === list.size)
    assertSegmentEquals(PLAIN, "hey ", list, 0)
    assertSegmentEquals(PLAIN, "\r\n\r\n", list, 1)
    assertSegmentEquals(PLAIN, "\r\n\r\n", list, 2)
    assertSegmentEquals(PLAIN, "\r\n\r\n", list, 3)
    assertSegmentEquals(PLAIN, " ho", list, 4)
  }

  behavior of "<hr>"

  it should "be parsed as line" in {
    val list = parse("hey <hr/> ho")
    assert(3 === list.size)
    assertSegmentEquals(PLAIN, "hey ", list, 0)
    assertSegmentEquals(LINE, null, list, 1)
    assertSegmentEquals(PLAIN, " ho", list, 2)
  }

  behavior of "<strong>"

  it should "be parsed as bold" in {
    val list = parse("hey <strong>fat</strong> ho")
    assert(3 === list.size)
    assertSegmentEquals(PLAIN, "hey ", list, 0)
    assertSegmentEquals(BOLD, null, list, 1)
    assertSegmentEquals(PLAIN, "fat", list, 1, 0)
    assertSegmentEquals(PLAIN, " ho", list, 2)
  }

  it should "be parsed at start" in {
    val list = parse("<strong>hey</strong> ho")
    assert(2 === list.size)
    assertSegmentEquals(BOLD, null, list, 0)
    assertSegmentEquals(PLAIN, "hey", list, 0, 0)
    assertSegmentEquals(PLAIN, " ho", list, 1)
  }

  it should "be parsed at end" in {
    val list = parse("hey <strong>ho</strong>")
    assert(2 === list.size)
    assertSegmentEquals(PLAIN, "hey ", list, 0)
    assertSegmentEquals(BOLD, null, list, 1)
    assertSegmentEquals(PLAIN, "ho", list, 1, 0)
  }

  it should "be ignored when empty" in {
    val list = parse("hey <strong></strong>")
    assert(1 === list.size)
    assertSegmentEquals(PLAIN, "hey ", list, 0)
  }

  behavior of "<em>"

  it should "be parsed as italics" in {
    val list = parse("hey <em>ho</em>")
    assert(2 === list.size)
    assertSegmentEquals(PLAIN, "hey ", list, 0)
    assertSegmentEquals(ITALICS, null, list, 1)
    assertSegmentEquals(PLAIN, "ho", list, 1, 0)
  }

  it should "allow formats inside" in {
    val list = parse("hey <em><strong>ho</strong></em>")
    assert(2 === list.size)
    assertSegmentEquals(PLAIN, "hey ", list, 0)
    assertSegmentEquals(ITALICS, null, list, 1)
    assertSegmentEquals(BOLD, null, list, 1, 0)
    assertSegmentEquals(PLAIN, "ho", list, 1, 0, 0)
  }

  behavior of "<hx>"

  it should "be parsed as heading" in {
    val list = parse("<h1>hey</h1>")
    assert(1 === list.size)
    assertSegmentEquals(HEADING, null, list, 0)
    assertSegmentEquals(PLAIN, "hey", list, 0, 0)
    assert(1 === list(0)(LEVEL).get)
  }

  it should "be be aware of level" in {
    val list = parse("<h2>hey</h2>")
    assert(1 === list.size)
    assert(2 === list(0)(LEVEL).get)
  }

  behavior of "<a>"

  it should "be parsed as link" in {
    val list = parse( """<a href="link"></a>""")
    assert(1 === list.size)
    assertSegmentEquals(LINK, null, list, 0)
    assertSegmentEquals(PLAIN, "link", list, 0, 0)
    assert("link" === list(0)(TARGET).get)
  }

  it should "accept description as child" in {
    val list = parse( """<a href="link">desc</a>""")
    assert(1 === list.size)
    assertSegmentEquals(LINK, null, list, 0)
    assertSegmentEquals(PLAIN, "desc", list, 0, 0)
    assert("link" === list(0)(TARGET).get)
  }

  it should "accept formats inside description" in {
    val list = parse( """<a href="link">desc<strong>bold</strong></a>""")
    assert(1 === list.size)
    assertSegmentEquals(LINK, null, list, 0)
    assertSegmentEquals(PLAIN, "desc", list, 0, 0)
    assertSegmentEquals(BOLD, null, list, 0, 1)
    assertSegmentEquals(PLAIN, "bold", list, 0, 1, 0)
    assert("link" === list(0)(TARGET).get)
  }

  it should "also be found in plaintext 'http://'" in {
    val list = parse("bla (http://hhhh) https://xxx http:/end")
    assert(5 === list.size)
    assertSegmentEquals(PLAIN, "bla (", list, 0)
    assertSegmentEquals(LINK, null, list, 1)
    assertSegmentEquals(PLAIN, "http://hhhh", list, 1, 0)
    assert("http://hhhh" === list(1)(TARGET).get)
    assertSegmentEquals(PLAIN, ") ", list, 2)
    assertSegmentEquals(LINK, null, list, 3)
    assertSegmentEquals(PLAIN, "https://xxx", list, 3, 0)
    assert("https://xxx" === list(3)(TARGET).get)
    assertSegmentEquals(PLAIN, " http:/end", list, 4)
  }

  behavior of "{{}}"

  it should "be parsed as image" in {
    val list = parse("{{link}}")
    assert(1 === list.size)
    assertSegmentEquals(IMAGE, null, list, 0)
    assertSegmentEquals(PLAIN, "link", list, 0, 0)
    assert("link" === list(0)(TARGET).get)
  }

  it should "accept description after |" in {
    val list = parse("{{link|desc}}")
    assert(1 === list.size)
    assertSegmentEquals(IMAGE, null, list, 0)
    assertSegmentEquals(PLAIN, "desc", list, 0, 0)
    assert("link" === list(0)(TARGET).get)
  }

  it should "accept 'angle' as customizer" in {
    val list = parse("{{<angle=90>link|desc**bold**}}")
    assert(1 === list.size)
    assertSegmentEquals(IMAGE, null, list, 0)
    assertSegmentEquals(PLAIN, "desc", list, 0, 0)
    assertSegmentEquals(BOLD, null, list, 0, 1)
    assertSegmentEquals(PLAIN, "bold", list, 0, 1, 0)
    assert("link" === list(0)(TARGET).get)
    assert("90" === list(0)(ANGLE).get)
  }

  it should "accept 'width' as customizer" in {
    val list = parse("{{<width=50%>link}}")
    assert(1 === list.size)
    assertSegmentEquals(IMAGE, null, list, 0)
    assertSegmentEquals(PLAIN, "link", list, 0, 0)
    assert("link" === list(0)(TARGET).get)
    assert("50%" === list(0)(WIDTH).get)
  }

  behavior of "<ol>"

  it should "be parsed as ordered list" in {
    val list = parse("hhh<ol><li>a</li><li>b</li></ol>next")
    assert(3 === list.size)
    assertSegmentEquals(PLAIN, "hhh", list, 0)
    assertSegmentEquals(LIST, null, list, 1)
    assert(ORDERED === list(1)(TYPE).get)
    assert(1 === list(1)(LEVEL).get)
    assertSegmentEquals(LIST_ITEM, null, list, 1, 0)
    assertSegmentEquals(PLAIN, "a", list, 1, 0, 0)
    assertSegmentEquals(LIST_ITEM, null, list, 1, 1)
    assertSegmentEquals(PLAIN, "b", list, 1, 1, 0)
    assertSegmentEquals(PLAIN, "next", list, 2)
  }

  behavior of "<ul>"

  it should "be parsed as unordered list" in {
    val list = parse("hhh<ul><li>a</li><li>b</li></ul>next")
    assert(3 === list.size)
    assertSegmentEquals(PLAIN, "hhh", list, 0)
    assertSegmentEquals(LIST, null, list, 1)
    assert(UNORDERED === list(1)(TYPE).get)
    assert(1 === list(1)(LEVEL).get)
    assertSegmentEquals(LIST_ITEM, null, list, 1, 0)
    assertSegmentEquals(PLAIN, "a", list, 1, 0, 0)
    assertSegmentEquals(LIST_ITEM, null, list, 1, 1)
    assertSegmentEquals(PLAIN, "b", list, 1, 1, 0)
    assertSegmentEquals(PLAIN, "next", list, 2)
  }

  it should "allow nested lists" in {
    val list = parse("hhh<ul><li>a<ul><li>b</li></ul></li><li>c</li></ul>next")
    assert(3 === list.size)
    assertSegmentEquals(LIST, null, list, 1)
    assert(UNORDERED === list(1)(TYPE).get)
    assert(1 === list(1)(LEVEL).get)
    assertSegmentEquals(LIST_ITEM, null, list, 1, 0)
    assertSegmentEquals(LIST, null, list, 1, 0, 1)
    val sublist = list(1).children(0).children(1)
    assert(UNORDERED === sublist(TYPE).get)
    assert(2 === sublist(LEVEL).get)
    assertSegmentEquals(LIST_ITEM, null, list, 1, 1)
  }

  it should "accept a sublist of <ol>" in {
    val list = parse("hhh<ul><li>a<ol><li>b</li></ol></li><li>c</li></ul>next")
    assert(3 === list.size)
    assertSegmentEquals(LIST, null, list, 1)
    assert(UNORDERED === list(1)(TYPE).get)
    assert(1 === list(1)(LEVEL).get)
    assertSegmentEquals(LIST_ITEM, null, list, 1, 0)
    assertSegmentEquals(LIST, null, list, 1, 0, 1)
    val sublist = list(1).children(0).children(1)
    assert(ORDERED === sublist(TYPE).get)
    assert(2 === sublist(LEVEL).get)
    assertSegmentEquals(LIST_ITEM, null, list, 1, 1)
  }

  behavior of "|"

  it should "parse as table" in {
    val list = parse("a|b")
    assert(1 === list.size)
    assertSegmentEquals(PLAIN, "a|b", list, 0)
  }

  it should "accept headings, formats inside, a caption" in {
    val list = parse("|=h1|=h2|\n   |a|b| \n |c \n |**d**e|f|g \n!<nonfloat>This table show interesting \"data\"\nnext")
    assert(2 === list.size)
    assertSegmentEquals(TABLE, null, list, 0)
    val table = list(0)
    assert(3 === table(COLUMNS).get)
    assert(4 === table(ROWS).get)
    assert(!table(FLOAT).get.asInstanceOf[Boolean])
    assert(12 === table.attributes.size)
    val c11 = table(Attribute("1,1")).get.asInstanceOf[Segment]
    assert(TABLE_CELL === c11.name)
    assert(true === c11(HEADER).get)
    assertSegmentEquals(PLAIN, "h1", c11.children, 0)
    val c12 = table(Attribute("1,2")).get.asInstanceOf[Segment]
    assert(TABLE_CELL === c12.name)
    assert(true === c12(HEADER).get)
    assertSegmentEquals(PLAIN, "h2", c12.children, 0)
    assert(table(Attribute("2,3")).isEmpty)
    assert(table(Attribute("3,2")).isEmpty)
    val c31 = table(Attribute("4,1")).get.asInstanceOf[Segment]
    assert(TABLE_CELL === c31.name)
    assert(c31(HEADER).isEmpty || !c31(HEADER).get.asInstanceOf[Boolean])

    assertSegmentEquals(BOLD, null, c31.children, 0)
    assertSegmentEquals(PLAIN, "d", c31.children, 0, 0)
    assertSegmentEquals(PLAIN, "e", c31.children, 1)
    assertSegmentEquals(PLAIN, "This table show interesting \"data\"", table(CAPTION).get.asInstanceOf[Segment])
    assertSegmentEquals(PLAIN, "next", list, 1)
  }


  it should "allow customizers for colspan, width, align" in {
    val list = parse("|<colspan=2>a|<width=5cm><align=right>b|<width=6pt><colspan=2>|")
    assert(1 === list.size)
    assertSegmentEquals(TABLE, null, list, 0)
    val table = list(0)
    assert(5 === table(COLUMNS).get)
    assert(1 === table(ROWS).get)
    assert(table(FLOAT).get.asInstanceOf[Boolean])
    assert(8 === table.attributes.size) //cols,rows,float, 3x cell,2x width
    assert(table(WIDTH.index(1)).isEmpty)
    assert("5cm" === table(WIDTH.index(3)).get)
    assert("6pt" === table(WIDTH.index(4)).get)
    val c11 = table(Attribute("1,1")).get.asInstanceOf[Segment]
    assert(2 === c11(SPAN).get)
    val c13 = table(Attribute("1,3")).get.asInstanceOf[Segment]
    assert(c13(SPAN).isEmpty)
    assert("right" === c13(ALIGN).get)
    val c14 = table(Attribute("1,4")).get.asInstanceOf[Segment]
    assert(2 === c14(SPAN).get)
  }

  behavior of "<,>,-,= combined to arrows"

  it should "parse as arrows" in {
    val list = parse("a-->b==>c<--d<==e<-->f<==>ggg->x=>x<-x<=x<->x<=>x")
    assert(13 === list.size)
    assertSegmentEquals(PLAIN, "a", list, 0)
    assertSegmentEquals(SYMBOL, null, list, 1)
    assert("-->" === list(1)(ORIGINAL).get)
    assert(ARROW_RIGHT === list(1)(TYPE).get)
    assertSegmentEquals(PLAIN, "b", list, 2)
    assertSegmentEquals(SYMBOL, null, list, 3)
    assert("==>" === list(3)(ORIGINAL).get)
    assert(DOUBLE_ARROW_RIGHT === list(3)(TYPE).get)
    assertSegmentEquals(PLAIN, "c", list, 4)
    assertSegmentEquals(SYMBOL, null, list, 5)
    assert("<--" === list(5)(ORIGINAL).get)
    assert(ARROW_LEFT === list(5)(TYPE).get)
    assertSegmentEquals(PLAIN, "d", list, 6)
    assertSegmentEquals(SYMBOL, null, list, 7)
    assert("<==" === list(7)(ORIGINAL).get)
    assert(DOUBLE_ARROW_LEFT === list(7)(TYPE).get)
    assertSegmentEquals(PLAIN, "e", list, 8)
    assertSegmentEquals(SYMBOL, null, list, 9)
    assert("<-->" === list(9)(ORIGINAL).get)
    assert(ARROW_BOTH === list(9)(TYPE).get)
    assertSegmentEquals(PLAIN, "f", list, 10)
    assertSegmentEquals(SYMBOL, null, list, 11)
    assert("<==>" === list(11)(ORIGINAL).get)
    assert(DOUBLE_ARROW_BOTH === list(11)(TYPE).get)
    assertSegmentEquals(PLAIN, "ggg->x=>x<-x<=x<->x<=>x", list, 12)
  }

  behavior of "unclosed formats"

  it should "be parsed as plain text" in {
    val list = parse("**hhh")
    assert(1 === list.size)
    assertSegmentEquals(BOLD, null, list, 0)
    assertSegmentEquals(PLAIN, "hhh", list, 0, 0)
  }
}
