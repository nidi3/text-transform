package stni.text.transform.parse.html

import stni.text.transform.Name._
import stni.text.transform.Attribute._
import stni.text.transform.AttributeValue._
import stni.text.transform.Segment._
import stni.text.transform.parse.ParserTest
import stni.text.transform.{Attribute, TransformContext}
import java.util.Locale


/**
 *
 */
class HtmlParserTest extends ParserTest {
  val parser = new HtmlParser(new TransformContext(0, Locale.GERMAN, null))

  "simple strings" should "be untouched" in {
    "" parseTo ROOT()
    "hey ho" parseTo plain("hey ho")
  }

  behavior of "<p>"

  ignore should "add a newline" in {
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

  behavior of "<font>"

  it should "be ignored" in {
    "hey <font>fat</font> ho" parseTo ROOT(plain("hey "), plain("fat"), plain(" ho"))
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

  ignore should "ingore unkonwn entities silently" in {
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

  it should "be aware of level" in {
    "<h2>hey</h2>" parseTo HEADING(plain("hey"), LEVEL -> 2)
  }

  it should "be parsed even with style attribute" in {
    "<h2 style=\"bla\">hey</h2>" parseTo HEADING(plain("hey"), LEVEL -> 2)
  }

  behavior of "<a>"

  it should "be parsed as link" in {
    """<a href="link"></a>""" parseTo LINK(CAPTION -> ROOT(plain("link")), TARGET -> "link", TYPE -> URL)
  }

  it should "accept description as child" in {
    """<a href="link">desc</a>""" parseTo LINK(CAPTION -> ROOT(plain("desc")), TARGET -> "link", TYPE -> URL)
  }

  it should "accept formats inside description" in {
    """<a href="link">desc<strong>bold</strong></a>""" parseTo LINK(CAPTION -> ROOT(plain("desc"), BOLD(plain("bold"))), TARGET -> "link", TYPE -> URL)
  }

  it should "create a reference to a label if href starts with #" in {
    "<a href='#pedro'>link</a>" parseTo LINK(TARGET -> "pedro", TYPE -> REF, CAPTION -> ROOT(plain("link")))
  }

  it should "also be found in plain 'http://'" in {
    "bla (http://hhhh) https://xxx http:/end" parseTo ROOT(
      plain("bla ("),
      LINK(CAPTION -> ROOT(plain("http://hhhh")), TARGET -> "http://hhhh", TYPE -> URL),
      plain(") "),
      LINK(CAPTION -> ROOT(plain("https://xxx")), TARGET -> "https://xxx", TYPE -> URL),
      plain(" http:/end"))
  }

  behavior of "<img>"

  it should "be parsed as image" in {
    "<img src='bla.png'/>" parseTo IMAGE(TARGET -> "bla.png")
    """<img src="bla.png"/>""" parseTo IMAGE(TARGET -> "bla.png")
  }

  it should "interpret the content of the alt attribute" in {
    "<img src='bla.png' alt='width: 15cm; caption: Super bild' />" parseTo
      IMAGE(TARGET -> "bla.png", WIDTH -> "15cm", CAPTION -> ROOT(plain("Super bild")))

    "<img src='bla.png' alt='caption: Super bild; width: 15cm;' />" parseTo
      IMAGE(TARGET -> "bla.png", WIDTH -> "15cm", CAPTION -> ROOT(plain("Super bild")))
  }

  it should "take the id attribute as label" in {
    "<img id='pedro'/>" parseTo IMAGE(TARGET -> "", ID -> "pedro")
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

  behavior of "<table>"

  it should "parse as table" in {
    "<table><tbody><tr><th>h1</th><th>h2</th></tr><tr><td>c1</td><td>c2</td></tr></tbody></table>" parseTo TABLE(
      COLUMNS -> 2, ROWS -> 2,
      Attribute("1,1") -> TABLE_CELL(HEADER -> true, plain("h1")),
      Attribute("1,2") -> TABLE_CELL(HEADER -> true, plain("h2")),
      Attribute("2,1") -> TABLE_CELL(plain("c1")),
      Attribute("2,2") -> TABLE_CELL(plain("c2")))
  }

  it should "interpet <td class='highlight'> as <th>" in {
    "<table><tbody><tr><td class='highlight'>h1</td><th>h2</th></tr><tr><td>c1</td><td>c2</td></tr></tbody></table>" parseTo TABLE(
      COLUMNS -> 2, ROWS -> 2,
      Attribute("1,1") -> TABLE_CELL(HEADER -> true, plain("h1")),
      Attribute("1,2") -> TABLE_CELL(HEADER -> true, plain("h2")),
      Attribute("2,1") -> TABLE_CELL(plain("c1")),
      Attribute("2,2") -> TABLE_CELL(plain("c2")))
  }

  it should "interpret the widths in px if given for all columns" in {
    "<table><tbody><tr><th style='width: 100px'>h1</th><th style='width: 300px'>h2</th></tr></tbody></table>" parseTo TABLE(
      COLUMNS -> 2, ROWS -> 1, WIDTH(1) -> "25.0%", WIDTH(2) -> "75.0%",
      Attribute("1,1") -> TABLE_CELL(HEADER -> true, plain("h1")),
      Attribute("1,2") -> TABLE_CELL(HEADER -> true, plain("h2")))
  }

  it should "understand a <caption> child element" in {
    "<table><caption>Hi <strong>fat</strong></caption><tbody><tr><td></td></tr></tbody></table>" parseTo TABLE(
      COLUMNS -> 1, ROWS -> 1,
      Attribute("1,1") -> TABLE_CELL(),
      CAPTION -> ROOT(plain("Hi "), BOLD(plain("fat"))))
  }



  //
  //
  //  it should "allow customizers for colspan, width, align" in {
  //    "|<colspan=2>a|<width=5cm><align=right>b|<width=6pt><colspan=2>|" parseTo  ROOT(
  //      assert(1 === list.size)
  //        assertSegmentEquals(TABLE, null, list, 0)
  //    val table = list(0)
  //    assert(5 === table(COLUMNS).get)
  //    assert(1 === table(ROWS).get)
  //    assert(table(FLOAT).get)
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

  behavior of "unclosed formats"

  ignore should "be parsed as plain text" in {
    "**hhh" parseTo BOLD(plain("hhh"))
  }

  behavior of "cleanNewlines"

  it should "clean up NEWLINES before/after HEADINGS, TABLES, LISTS" in {
    "a<br/><br/><h1>head</h1><br/><br/>b" parseTo ROOT(plain("a"), HEADING(LEVEL -> 1, plain("head")), plain("b"))
    "<strong>a<br/><br/><h1>head</h1><br/><br/>b</strong>" parseTo ROOT(BOLD(plain("a"), HEADING(LEVEL -> 1, plain("head")), plain("b")))
    "a<br/><br/><table></table><br/><br/>b" parseTo ROOT(plain("a"), TABLE(COLUMNS -> 0, ROWS -> 0), plain("b"))
    "a<br/><br/><ul><li></li></ul><br/><br/>b" parseTo ROOT(plain("a"), LIST(LEVEL -> 1, TYPE -> UNORDERED), plain("b"))
  }

  it should "clean up NEWLINES at start and end" in {
    "<br/><br/>a" parseTo plain("a")
    "a<br/><br/>" parseTo plain("a")
    "<br/><br/>a<br/><br/>" parseTo ROOT(plain("a"))
  }

  behavior of "-> etc."

  it should "be displayed as an arrow" in {
    "-&gt;" parseTo ROOT(symbol("->", ARROW_RIGHT))
    "&lt;-" parseTo ROOT(symbol("<-", ARROW_LEFT))
    "=&gt;" parseTo ROOT(symbol("=>", DOUBLE_ARROW_RIGHT))
    "&lt;=" parseTo ROOT(symbol("<=", DOUBLE_ARROW_LEFT))
    "&lt;-&gt;" parseTo ROOT(symbol("<->", ARROW_BOTH))
    "&lt;=&gt;" parseTo ROOT(symbol("<=>", DOUBLE_ARROW_BOTH))
  }

  it should "be parsed with text around" in {
    "hallo -&gt; velo" parseTo ROOT(plain("hallo "), symbol("->", ARROW_RIGHT), plain(" velo"))
  }
}
