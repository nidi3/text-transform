package org.mimacom.text.transform.parse.wiki.creole

import org.mimacom.text.transform.Name._
import org.scalatest.FlatSpec
import org.mimacom.text.transform.{AttributeValue, Attribute, Segment, Name}
import scala.Predef._
import org.mimacom.text.transform.Attribute._
import org.mimacom.text.transform.AttributeValue._


/**
 *
 */
class CreoleWikiParserTest extends FlatSpec {
  val parser = new CreoleWikiParser

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

  behavior of "**"

  it should "be parsed as bold" in {
    val list = parse("hey **fat** ho")
    assert(3 === list.size)
    assertSegmentEquals(PLAIN, "hey ", list, 0)
    assertSegmentEquals(BOLD, null, list, 1)
    assertSegmentEquals(PLAIN, "fat", list, 1, 0)
    assertSegmentEquals(PLAIN, " ho", list, 2)
  }

  it should "be parsed at start" in {
    val list = parse("**hey** ho")
    assert(2 === list.size)
    assertSegmentEquals(BOLD, null, list, 0)
    assertSegmentEquals(PLAIN, "hey", list, 0, 0)
    assertSegmentEquals(PLAIN, " ho", list, 1)
  }

  it should "be parsed at end" in {
    val list = parse("hey **ho**")
    assert(2 === list.size)
    assertSegmentEquals(PLAIN, "hey ", list, 0)
    assertSegmentEquals(BOLD, null, list, 1)
    assertSegmentEquals(PLAIN, "ho", list, 1, 0)
  }

  it should "be ignored when empty" in {
    val list = parse("hey ****")
    assert(1 === list.size)
    assertSegmentEquals(PLAIN, "hey ", list, 0)
  }

  it should "be ignored when only one *" in {
    val list = parse("hey *ho*")
    assert(1 === list.size)
    assertSegmentEquals(PLAIN, "hey *ho*", list, 0)
  }

  it should "ignore * inside" in {
    val list = parse("hey **a*h*o**")
    assert(2 === list.size)
    assertSegmentEquals(PLAIN, "hey ", list, 0)
    assertSegmentEquals(BOLD, null, list, 1)
    assertSegmentEquals(PLAIN, "a*h*o", list, 1, 0)
  }

  behavior of "//"

  it should "be parsed as italics" in {
    val list = parse("hey //ho//")
    assert(2 === list.size)
    assertSegmentEquals(PLAIN, "hey ", list, 0)
    assertSegmentEquals(ITALICS, null, list, 1)
    assertSegmentEquals(PLAIN, "ho", list, 1, 0)
  }

  it should "be parsed when prepended by :" in {
    val list = parse("hey ://ho//")
    assert(2 === list.size)
    assertSegmentEquals(PLAIN, "hey :", list, 0)
    assertSegmentEquals(ITALICS, null, list, 1)
    assertSegmentEquals(PLAIN, "ho", list, 1, 0)
  }

  it should "be parsed as link when prepended by http:" in {
    val list = parse("hey http://ho")
    assert(2 === list.size)
    assertSegmentEquals(PLAIN, "hey ", list, 0)
    assertSegmentEquals(LINK, null, list, 1)
    assert("http://ho" === list(1)(TARGET).get)
    assertSegmentEquals(PLAIN, "http://ho", list, 1, 0)
  }

  it should "be parsed as link when prepended by https:" in {
    val list = parse("hey https://ho")
    assert(2 === list.size)
    assertSegmentEquals(PLAIN, "hey ", list, 0)
    assertSegmentEquals(LINK, null, list, 1)
    assert("https://ho" === list(1)(TARGET).get)
    assertSegmentEquals(PLAIN, "https://ho", list, 1, 0)
  }

  it should "allow formats inside" in {
    val list = parse("hey //**ho**//")
    assert(2 === list.size)
    assertSegmentEquals(PLAIN, "hey ", list, 0)
    assertSegmentEquals(ITALICS, null, list, 1)
    assertSegmentEquals(BOLD, null, list, 1, 0)
    assertSegmentEquals(PLAIN, "ho", list, 1, 0, 0)
  }

  behavior of "="

  it should "be parsed as heading" in {
    val list = parse("=hey=")
    assert(1 === list.size)
    assertSegmentEquals(HEADING, null, list, 0)
    assertSegmentEquals(PLAIN, "hey", list, 0, 0)
    assert(1 === list(0)(LEVEL).get)
  }

  it should "ignore fewer = inside" in {
    val list = parse("====he==y====")
    assert(1 === list.size)
    assertSegmentEquals(HEADING, null, list, 0)
    assertSegmentEquals(PLAIN, "he==y", list, 0, 0)
    assert(4 === list(0)(LEVEL).get)
  }

  it should "ignore formats inside" in {
    val list = parse("=hey**bold**=")
    assert(1 === list.size)
    assertSegmentEquals(HEADING, null, list, 0)
    assertSegmentEquals(PLAIN, "hey**bold**", list, 0, 0)
  }

  it should "end at newline" in {
    val list = parse("==he==\nd")
    assert(2 === list.size)
    assertSegmentEquals(HEADING, null, list, 0)
    assertSegmentEquals(PLAIN, "he", list, 0, 0)
    assertSegmentEquals(PLAIN, "d", list, 1)
  }

  it should "end at newline even without ending =" in {
    val list = parse("=hey\nnew")
    assert(2 === list.size)
    assertSegmentEquals(HEADING, null, list, 0)
    assertSegmentEquals(PLAIN, "hey", list, 0, 0)
    assertSegmentEquals(PLAIN, "new", list, 1)
  }

  it should "ignore more than one newline at end" in {
    val list = parse("==he==\n\n")
    assert(1 === list.size)
    assertSegmentEquals(HEADING, null, list, 0)
    assertSegmentEquals(PLAIN, "he", list, 0, 0)
  }

  it should "ignore more than one newline" in {
    val list = parse("==he==\n\n\nd\n")
    assert(2 === list.size)
    assertSegmentEquals(HEADING, null, list, 0)
    assertSegmentEquals(PLAIN, "he", list, 0, 0)
    assertSegmentEquals(PLAIN, "d\n", list, 1)
  }

  it should "ignore whitespaces directly before newlines" in {
    val list = parse("==he==  \n\n\n\n")
    assert(1 === list.size)
    assertSegmentEquals(HEADING, null, list, 0)
    assertSegmentEquals(PLAIN, "he", list, 0, 0)
  }

  behavior of "[[]]"

  it should "be parsed as link" in {
    val list = parse("[[link]]")
    assert(1 === list.size)
    assertSegmentEquals(LINK, null, list, 0)
    assertSegmentEquals(PLAIN, "link", list, 0, 0)
    assert("link" === list(0)(TARGET).get)
  }

  it should "accept description after |" in {
    val list = parse("[[link|desc]]")
    assert(1 === list.size)
    assertSegmentEquals(LINK, null, list, 0)
    assertSegmentEquals(PLAIN, "desc", list, 0, 0)
    assert("link" === list(0)(TARGET).get)
  }

  it should "accept formats inside description" in {
    val list = parse("[[link|desc**bold**]]")
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

  behavior of "#"

  it should "be ignored when not at start of line" in {
    val list = parse("hhh#hhh")
    assert(1 === list.size)
    assertSegmentEquals(PLAIN, "hhh#hhh", list, 0)
  }

  it should "be parsed as ordered list when at start of line" in {
    val list = parse("hhh\n  #a\nb\n#c\n\nnext")
    assert(3 === list.size)
    assertSegmentEquals(PLAIN, "hhh\n", list, 0)
    assertSegmentEquals(LIST, null, list, 1)
    assert(ORDERED === list(1)(TYPE).get)
    assert(1 === list(1)(LEVEL).get)
    assertSegmentEquals(LIST_ITEM, null, list, 1, 0)
    assertSegmentEquals(PLAIN, "a\nb", list, 1, 0, 0)
    assertSegmentEquals(LIST_ITEM, null, list, 1, 1)
    assertSegmentEquals(PLAIN, "c", list, 1, 1, 0)
    assertSegmentEquals(PLAIN, "next", list, 2)
  }

  behavior of "*"

  it should "be parsed as unordered list when at start of line" in {
    val list = parse("hhh\n  *a\nb\n*c\n\nnext")
    assert(3 === list.size)
    assertSegmentEquals(PLAIN, "hhh\n", list, 0)
    assertSegmentEquals(LIST, null, list, 1)
    assert(UNORDERED === list(1)(TYPE).get)
    assert(1 === list(1)(LEVEL).get)
    assertSegmentEquals(LIST_ITEM, null, list, 1, 0)
    assertSegmentEquals(PLAIN, "a\nb", list, 1, 0, 0)
    assertSegmentEquals(LIST_ITEM, null, list, 1, 1)
    assertSegmentEquals(PLAIN, "c", list, 1, 1, 0)
    assertSegmentEquals(PLAIN, "next", list, 2)
  }

  it should "create a sublist for more than one *" in {
    val list = parse("hhh\n  *a\n**b\n*c\n\nnext")
    assert(3 === list.size)
    assertSegmentEquals(LIST, null, list, 1)
    assert(UNORDERED === list(1)(TYPE).get)
    assert(1 === list(1)(LEVEL).get)
    assertSegmentEquals(LIST_ITEM, null, list, 1, 0)
    assertSegmentEquals(LIST, null, list, 1, 1)
    val sublist = list(1).children(1)
    assert(UNORDERED === sublist(TYPE).get)
    assert(2 === sublist(LEVEL).get)
    assertSegmentEquals(LIST_ITEM, null, list, 1, 1, 0)
  }

  it should "create an empty sublist if one level is omitted" in {
    val list = parse("hhh\n  *a\n***b\n*c\n\nnext")
    assert(3 === list.size)
    assertSegmentEquals(LIST, null, list, 1)
    assertSegmentEquals(LIST, null, list, 1, 1)
    assertSegmentEquals(LIST, null, list, 1, 1, 0)
    val sublist = list(1).children(1).children(0)
    assert(UNORDERED === sublist(TYPE).get)
    assert(3 === sublist(LEVEL).get)
    assertSegmentEquals(LIST_ITEM, null, list, 1, 1, 0, 0)
  }

  it should "accept a sublist of #" in {
    val list = parse("hhh\n  *a\n##b\n*c\n\nnext")
    assert(3 === list.size)
    assertSegmentEquals(LIST, null, list, 1)
    assert(UNORDERED === list(1)(TYPE).get)
    assert(1 === list(1)(LEVEL).get)
    assertSegmentEquals(LIST_ITEM, null, list, 1, 0)
    assertSegmentEquals(LIST, null, list, 1, 1)
    val sublist = list(1).children(1)
    assert(ORDERED === sublist(TYPE).get)
    assert(2 === sublist(LEVEL).get)
    assertSegmentEquals(LIST_ITEM, null, list, 1, 1, 0)
  }

  it should "end when a list of # begins" in {
    val list = parse("*a\n#b\n")
    assert(2 === list.size)
    assertSegmentEquals(LIST, null, list, 0)
    assert(UNORDERED === list(0)(TYPE).get)
    assert(1 === list(0)(LEVEL).get)
    assertSegmentEquals(LIST, null, list, 1)
    assert(ORDERED === list(1)(TYPE).get)
    assert(1 === list(1)(LEVEL).get)
  }

  it should "not keep state between to executions" in {
    val list = parse("#a")
    assert(1 === list.size)
    val list2 = parse("#a")
    assert(1 === list2.size)
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

  behavior of "----"

  it should "be plain text if shorter than 4" in {
    val list = parse("---bla")
    assert(1 === list.size)
    assertSegmentEquals(PLAIN, "---bla", list, 0)
  }

  it should "be parsed as a line if at least 4 length" in {
    val list = parse("----bla")
    assert(2 === list.size)
    assertSegmentEquals(LINE, null, list, 0)
    assertSegmentEquals(PLAIN, "bla", list, 1)
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
