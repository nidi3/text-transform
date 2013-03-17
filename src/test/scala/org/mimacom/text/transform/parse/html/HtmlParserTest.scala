package org.mimacom.text.transform.parse.html

import org.mimacom.text.transform.Name._
import org.mimacom.text.transform.Attribute._
import org.mimacom.text.transform.AttributeValue._
import org.mimacom.text.transform.Segment._
import org.mimacom.text.transform.parse.ParserTest


/**
 *
 */
class HtmlParserTest extends ParserTest {
  val parser = new HtmlParser

  "simple strings" should "be untouched" in {
    "" parseTo ROOT()
    "hey ho" parseTo plain("hey ho")
  }

  behavior of "<p>"

  it should "add a newline" in {
    "hey <p>fat</p> ho" parseTo ROOT(plain("hey "), NEWLINE(), plain("fat"), NEWLINE(), plain(" ho"))
  }

  behavior of "<span>"

  it should "be ignored" in {
    "hey <span>fat</span> ho" parseTo ROOT(plain("hey "), plain("fat"), plain(" ho"))
  }

  behavior of "<div>"

  it should "be ignored" in {
    "hey <div>fat</div> ho" parseTo ROOT(plain("hey "), plain("fat"), plain(" ho"))
  }

  behavior of "<br>"

  it should "add a newline" in {
    "hey <br/><br /><br></br> ho" parseTo ROOT(plain("hey "), NEWLINE(), NEWLINE(), NEWLINE(), plain(" ho"))
  }

  behavior of "<hr>"

  it should "be parsed as line" in {
    "hey <hr/> ho" parseTo ROOT(plain("hey "), LINE(), plain(" ho"))
  }

  behavior of "<strong>"

  it should "be parsed as bold" in {
    "hey <strong>fat</strong> ho" parseTo ROOT(plain("hey "), BOLD(plain("fat")), plain(" ho"))
  }

  it should "be parsed at start" in {
    "<strong>hey</strong> ho" parseTo ROOT(BOLD(plain("hey")), plain(" ho"))
  }

  it should "be parsed at end" in {
    "hey <strong>ho</strong>" parseTo ROOT(plain("hey "), BOLD(plain("ho")))
  }

  it should "be ignored when empty" in {
    "hey <strong></strong>" parseTo plain("hey ")
  }

  behavior of "entities"

  it should "ingore unkonwn entities silently" in {
    "xx&theta;yy" parseTo plain("xxyy")
  }

  behavior of "<em>"

  it should "be parsed as italics" in {
    "hey <em>ho</em>" parseTo ROOT(plain("hey "), ITALICS(plain("ho")))
  }

  it should "allow formats inside" in {
    "hey <em><strong>ho</strong></em>" parseTo ROOT(plain("hey "), ITALICS(BOLD(plain("ho"))))
  }

  behavior of "<hx>"

  it should "be parsed as heading" in {
    "<h1>hey</h1>" parseTo HEADING(plain("hey"), LEVEL -> 1)
  }

  it should "be be aware of level" in {
    "<h2>hey</h2>" parseTo HEADING(plain("hey"), LEVEL -> 2)
  }

  behavior of "<a>"

  it should "be parsed as link" in {
    """<a href="link"></a>""" parseTo LINK(plain("link"), TARGET -> "link", TYPE -> URL)
  }

  it should "accept description as child" in {
    """<a href="link">desc</a>""" parseTo LINK(plain("desc"), TARGET -> "link", TYPE -> URL)
  }

  it should "accept formats inside description" in {
    """<a href="link">desc<strong>bold</strong></a>""" parseTo LINK(plain("desc"), BOLD(plain("bold")), TARGET -> "link", TYPE -> URL)
  }

  it should "also be found in plain 'http://'" in {
    "bla (http://hhhh) https://xxx http:/end" parseTo ROOT(
      plain("bla ("),
      LINK(plain("http://hhhh"), TARGET -> "http://hhhh", TYPE -> URL),
      plain(") "),
      LINK(plain("https://xxx"), TARGET -> "https://xxx", TYPE -> URL),
      plain(" http:/end"))
  }

  behavior of "{{}}"

  it should "be parsed as image" in {
    "{{link}}" parseTo IMAGE(plain("link"), TARGET -> "link")
  }

  it should "accept description after |" in {
    "{{link|desc}}" parseTo IMAGE(plain("desc"), TARGET -> "link")
  }

  it should "accept 'angle' as customizer" in {
    "{{<angle=90>link|desc**bold**}}" parseTo
      IMAGE(plain("desc"), BOLD(plain("bold")), TARGET -> "link", ANGLE -> "90")
  }

  it should "accept 'width' as customizer" in {
    "{{<width=50%>link}}" parseTo
      IMAGE(plain("link"), TARGET -> "link", WIDTH -> "50%")
  }

  behavior of "<ol>"

  it should "be parsed as ordered list" in {
    "hhh<ol><li>a</li><li>b</li></ol>next" parseTo ROOT(
      plain("hhh"),
      LIST(TYPE -> ORDERED, LEVEL -> 1, ITEM(plain("a")), ITEM(plain("b"))),
      plain("next"))
  }

  behavior of "<ul>"

  it should "be parsed as unordered list" in {
    "hhh<ul><li>a</li><li>b</li></ul>next" parseTo ROOT(
      plain("hhh"),
      LIST(TYPE -> UNORDERED, LEVEL -> 1, ITEM(plain("a")), ITEM(plain("b"))),
      plain("next"))
  }

  it should "allow nested lists" in {
    "hhh<ul><li>a<ul><li>b</li></ul></li><li>c</li></ul>next" parseTo ROOT(
      plain("hhh"),
      LIST(TYPE -> UNORDERED, LEVEL -> 1,
        ITEM(plain("a"), LIST(
          TYPE -> UNORDERED, LEVEL -> 2, ITEM(plain("b")))),
        ITEM(plain("c"))),
      plain("next"))
  }

  it should "accept a sublist of <ol>" in {
    "hhh<ul><li>a<ol><li>b</li></ol></li><li>c</li></ul>next" parseTo ROOT(
      plain("hhh"),
      LIST(TYPE -> UNORDERED, LEVEL -> 1,
        ITEM(plain("a"), LIST(
          TYPE -> ORDERED, LEVEL -> 2, ITEM(plain("b")))),
        ITEM(plain("c"))),
      plain("next"))
  }

  behavior of "|"

  //  it should "parse as table" in {
  //    "a|b" parseTo  ROOT(
  //      assert(1 === list.size)
  //        assertSegmentEquals(PLAIN, "a|b", list, 0)
  //  }
  //
  //  it should "accept headings, formats inside, a caption" in {
  //    "|=h1|=h2|\n   |a|b| \n |c \n |**d**e|f|g \n!<nonfloat>This table show interesting \"data\"\nnext" parseTo  ROOT(
  //      assert(2 === list.size)
  //        assertSegmentEquals(TABLE, null, list, 0)
  //    val table = list(0)
  //    assert(3 === table(COLUMNS).get)
  //    assert(4 === table(ROWS).get)
  //    assert(!table(FLOAT).get.asInstanceOf[Boolean])
  //    assert(12 === table.attributes.size)
  //    val c11 = table(Attribute("1,1")).get.asInstanceOf[Segment]
  //    assert(TABLE_CELL === c11.name)
  //    assert(true === c11(HEADER).get)
  //    assertSegmentEquals(PLAIN, "h1", c11.children, 0)
  //    val c12 = table(Attribute("1,2")).get.asInstanceOf[Segment]
  //    assert(TABLE_CELL === c12.name)
  //    assert(true === c12(HEADER).get)
  //    assertSegmentEquals(PLAIN, "h2", c12.children, 0)
  //    assert(table(Attribute("2,3")).isEmpty)
  //    assert(table(Attribute("3,2")).isEmpty)
  //    val c31 = table(Attribute("4,1")).get.asInstanceOf[Segment]
  //    assert(TABLE_CELL === c31.name)
  //    assert(c31(HEADER).isEmpty || !c31(HEADER).get.asInstanceOf[Boolean])
  //
  //    assertSegmentEquals(BOLD, null, c31.children, 0)
  //    assertSegmentEquals(PLAIN, "d", c31.children, 0, 0)
  //    assertSegmentEquals(PLAIN, "e", c31.children, 1)
  //    assertSegmentEquals(PLAIN, "This table show interesting \"data\"", table(CAPTION).get.asInstanceOf[Segment])
  //    assertSegmentEquals(PLAIN, "next", list, 1)
  //  }
  //
  //
  //  it should "allow customizers for colspan, width, align" in {
  //    "|<colspan=2>a|<width=5cm><align=right>b|<width=6pt><colspan=2>|" parseTo  ROOT(
  //      assert(1 === list.size)
  //        assertSegmentEquals(TABLE, null, list, 0)
  //    val table = list(0)
  //    assert(5 === table(COLUMNS).get)
  //    assert(1 === table(ROWS).get)
  //    assert(table(FLOAT).get.asInstanceOf[Boolean])
  //    assert(8 === table.attributes.size) //cols,rows,float, 3x cell,2x width
  //    assert(table(WIDTH.index(1)).isEmpty)
  //    assert("5cm" === table(WIDTH.index(3)).get)
  //    assert("6pt" === table(WIDTH.index(4)).get)
  //    val c11 = table(Attribute("1,1")).get.asInstanceOf[Segment]
  //    assert(2 === c11(SPAN).get)
  //    val c13 = table(Attribute("1,3")).get.asInstanceOf[Segment]
  //    assert(c13(SPAN).isEmpty)
  //    assert("right" === c13(ALIGN).get)
  //    val c14 = table(Attribute("1,4")).get.asInstanceOf[Segment]
  //    assert(2 === c14(SPAN).get)
  //  }

  //  behavior of "<,>,-,= combined to arrows"
  //
  //  it should "parse as arrows" in {
  //    "a-->b==>c<--d<==e<-->f<==>ggg->x=>x<-x<=x<->x<=>x" parseTo  ROOT(
  //      assert(13 === list.size)
  //        assertSegmentEquals(PLAIN, "a", list, 0)
  //        assertSegmentEquals(SYMBOL, null, list, 1)
  //        assert ("-->" === list(1)(ORIGINAL).get)
  //        assert (ARROW_RIGHT === list(1)(TYPE).get)
  //        assertSegmentEquals(PLAIN, "b", list, 2)
  //        assertSegmentEquals(SYMBOL, null, list, 3)
  //        assert ("==>" === list(3)(ORIGINAL).get)
  //        assert (DOUBLE_ARROW_RIGHT === list(3)(TYPE).get)
  //        assertSegmentEquals(PLAIN, "c", list, 4)
  //        assertSegmentEquals(SYMBOL, null, list, 5)
  //        assert ("<--" === list(5)(ORIGINAL).get)
  //        assert (ARROW_LEFT === list(5)(TYPE).get)
  //        assertSegmentEquals(PLAIN, "d", list, 6)
  //        assertSegmentEquals(SYMBOL, null, list, 7)
  //        assert ("<==" === list(7)(ORIGINAL).get)
  //        assert (DOUBLE_ARROW_LEFT === list(7)(TYPE).get)
  //        assertSegmentEquals(PLAIN, "e", list, 8)
  //        assertSegmentEquals(SYMBOL, null, list, 9)
  //        assert ("<-->" === list(9)(ORIGINAL).get)
  //        assert (ARROW_BOTH === list(9)(TYPE).get)
  //        assertSegmentEquals(PLAIN, "f", list, 10)
  //        assertSegmentEquals(SYMBOL, null, list, 11)
  //        assert ("<==>" === list(11)(ORIGINAL).get)
  //        assert (DOUBLE_ARROW_BOTH === list(11)(TYPE).get)
  //        assertSegmentEquals(PLAIN, "ggg->x=>x<-x<=x<->x<=>x", list, 12)
  //  }

  behavior of "unclosed formats"

  it should "be parsed as plain text" in {
    "**hhh" parseTo BOLD(plain("hhh"))
  }
}
