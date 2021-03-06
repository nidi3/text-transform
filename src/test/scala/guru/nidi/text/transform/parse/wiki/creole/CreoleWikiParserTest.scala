/**
 * Copyright (C) 2013 Stefan Niederhauser (nidin@gmx.ch)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package guru.nidi.text.transform.parse.wiki.creole

import java.util.Locale

import guru.nidi.text.transform.Attribute._
import guru.nidi.text.transform.AttributeValue._
import guru.nidi.text.transform.Name._
import guru.nidi.text.transform.Segment._
import guru.nidi.text.transform.{Attribute, TransformContext}
import guru.nidi.text.transform.parse.ParserTest


/**
 *
 */
class CreoleWikiParserTest extends ParserTest {
  val parser = new CreoleWikiParser(new TransformContext(0, Locale.GERMAN, null))

  "simple strings" should "be untouched" in {
    "" parseTo ROOT()
    "hey ho" parseTo plain("hey ho")
  }

  behavior of "**"

  it should "be parsed as bold" in {
    "hey **fat** ho" parseTo ROOT(plain("hey "), BOLD(plain("fat")), plain(" ho"))
  }

  it should "be parsed at start" in {
    "**hey** ho" parseTo ROOT(BOLD(plain("hey")), plain(" ho"))
  }

  it should "be parsed at end" in {
    "hey **ho**" parseTo ROOT(plain("hey "), BOLD(plain("ho")))
  }

  it should "be ignored when empty" in {
    "hey ****" parseTo plain("hey ")
  }

  it should "be ignored when only one *" in {
    "hey *ho*" parseTo plain("hey *ho*")
  }

  it should "ignore * inside" in {
    "hey **a*h*o**" parseTo ROOT(plain("hey "), BOLD(plain("a*h*o")))
  }

  behavior of "//"

  it should "be parsed as italics" in {
    "hey //ho//" parseTo ROOT(plain("hey "), ITALICS(plain("ho")))
  }

  it should "be parsed when prepended by :" in {
    "hey ://ho//" parseTo ROOT(plain("hey :"), ITALICS(plain("ho")))
  }

  it should "be parsed as link when prepended by http:" in {
    "hey http://ho.com" parseTo ROOT(plain("hey "), LINK(TYPE -> URL, TARGET -> "http://ho.com", CAPTION -> ROOT(plain("http://ho.com"))))
  }

  it should "be parsed as link when prepended by https:" in {
    "hey https://ho.com" parseTo ROOT(plain("hey "), LINK(TYPE -> URL, TARGET -> "https://ho.com", CAPTION -> ROOT(plain("https://ho.com"))))
  }

  it should "allow formats inside" in {
    "hey //**ho**//" parseTo ROOT(plain("hey "), ITALICS(BOLD(plain("ho"))))
  }

  behavior of "="

  it should "be parsed as heading" in {
    "=hey=" parseTo HEADING(LEVEL -> 1, plain("hey"))
  }

  it should "trim whitespaces" in {
    "=  hey  =" parseTo HEADING(LEVEL -> 1, plain("hey"))
  }

  it should "ignore fewer = inside" in {
    "====he==y====" parseTo HEADING(LEVEL -> 4, plain("he==y"))
  }

  it should "ignore formats inside" in {
    "=hey**bold**=" parseTo HEADING(LEVEL -> 1, plain("hey**bold**"))
  }

  it should "end at newline" in {
    "==he==\nd" parseTo ROOT(HEADING(LEVEL -> 2, plain("he")), plain("d"))
  }

  it should "end at newline even without ending =" in {
    "=hey\nnew" parseTo ROOT(HEADING(LEVEL -> 1, plain("hey")), plain("new"))
  }

  it should "ignore more than one newline at end" in {
    "==he==\n\n" parseTo HEADING(LEVEL -> 2, plain("he"))
  }

  it should "ignore more than one newline" in {
    "==he==\n\n\nd\n" parseTo ROOT(HEADING(LEVEL -> 2, plain("he")), plain("d\n"))
  }

  it should "ignore whitespaces directly before newlines" in {
    "==he==  \n\n\n\n" parseTo HEADING(LEVEL -> 2, plain("he"))
  }

  behavior of "[[]]"

  it should "be parsed as link" in {
    "[[link]]" parseTo LINK(TYPE -> URL, TARGET -> "link", CAPTION -> ROOT(plain("link")))
  }

  it should "accept description after |" in {
    "[[link|desc]]" parseTo LINK(TYPE -> URL, TARGET -> "link", CAPTION -> ROOT(plain("desc")))
  }

  it should "accept formats inside description" in {
    "[[link|desc**bold**]]" parseTo LINK(TYPE -> URL, TARGET -> "link", CAPTION -> ROOT(plain("desc"), BOLD(plain("bold"))))
  }

  it should "treat links starting with 'image:' specially" in {
    "[[image:xxx]]" parseTo LINK(TYPE -> IMAGE_REF, TARGET -> "xxx", CAPTION -> ROOT(plain("xxx")))
  }

  it should "treat links starting with 'document:' specially" in {
    "[[document:xxx]]" parseTo LINK(TYPE -> DOCUMENT_REF, TARGET -> "xxx", CAPTION -> ROOT(plain("xxx")))
  }

  it should "also be found in plaintext 'http://'" in {
    "bla (http://hhhh.com) https://xxx.com http:/end" parseTo ROOT(
      plain("bla ("),
      LINK(TYPE -> URL, TARGET -> "http://hhhh.com", CAPTION -> ROOT(plain("http://hhhh.com"))),
      plain(") "),
      LINK(TYPE -> URL, TARGET -> "https://xxx.com", CAPTION -> ROOT(plain("https://xxx.com"))),
      plain(" http:/end"))
  }

  behavior of "{{}}"

  it should "be parsed as image" in {
    "{{link}}" parseTo IMAGE(FLOAT -> true, TARGET -> "link", CAPTION -> ROOT(plain("link")))
  }

  it should "accept description after |" in {
    "{{link|desc}}" parseTo IMAGE(FLOAT -> true, TARGET -> "link", CAPTION -> ROOT(plain("desc")))
  }

  it should "accept 'angle' as customizer" in {
    "{{<angle=90>link|desc**bold**}}" parseTo IMAGE(
      FLOAT -> true, TARGET -> "link", ANGLE -> "90", CAPTION -> ROOT(plain("desc"), BOLD(plain("bold"))))
  }

  it should "accept 'width' as customizer" in {
    "{{<width=50%>link}}" parseTo IMAGE(FLOAT -> true, TARGET -> "link", WIDTH -> "50%", CAPTION -> ROOT(plain("link")))
  }

  behavior of ";"

  it should "not be parsed when not at start of line" in {
    "a: \nb;" parseTo plain("a: \nb;")
  }

  it should "not be parsed when : comes without ;" in {
    ":nodef" parseTo plain(":nodef")
  }

  it should ": can be left out" in {
    ";nodef" parseTo DEFINITION(TEXT -> "nodef")
  }

  it should "ignore : if it does not follow immediately after ;" in {
    ";word \n\n :def" parseTo ROOT(DEFINITION(TEXT -> "word "), plain("\n:def"))
  }

  it should "be parsed as a definition when : follows immediately after ;" in {
    ";   word\n: def" parseTo DEFINITION(TEXT -> "word", ITEM(plain("def")))
  }

  it should "allow formatted definitions" in {
    ";   word\n: **def**" parseTo DEFINITION(TEXT -> "word", ITEM(BOLD(plain("def"))))
  }

  it should "allow multiple definitions" in {
    ";   word\n: **def**\n:def2" parseTo DEFINITION(TEXT -> "word", ITEM(BOLD(plain("def"))), ITEM(plain("def2")))
  }

  it should "allow width customization" in {
    ";<width=5cm>word\n:def" parseTo DEFINITION(TEXT -> "word", WIDTH -> "5cm", ITEM(plain("def")))
  }

  behavior of "#"

  it should "be ignored when not at start of line" in {
    "hhh#hhh" parseTo plain("hhh#hhh")
  }

  it should "be parsed as ordered list when at start of line" in {
    "hhh\n  #a\nb\n#c\n\nnext" parseTo ROOT(
      plain("hhh\n"),
      LIST(TYPE -> ORDERED, LEVEL -> 1, ITEM(plain("a\nb")), ITEM(plain("c"))),
      plain("next"))
  }

  it should "trim whitespaces" in {
    "# \ta    \n\nnext" parseTo ROOT(
      LIST(TYPE -> ORDERED, LEVEL -> 1, ITEM(plain("a"))),
      plain("next"))
  }

  behavior of "*"

  it should "be parsed as unordered list when at start of line" in {
    "hhh\n  *a\nb\n*c\n\nnext" parseTo ROOT(
      plain("hhh\n"),
      LIST(TYPE -> UNORDERED, LEVEL -> 1, ITEM(plain("a\nb")), ITEM(plain("c"))),
      plain("next"))
  }

  it should "trim whitespaces" in {
    "* \ta    \n\nnext" parseTo ROOT(
      LIST(TYPE -> UNORDERED, LEVEL -> 1, ITEM(plain("a"))),
      plain("next"))
  }

  it should "create a sublist for more than one *" in {
    "hhh\n  *a\n**b\n*c\n\nnext" parseTo ROOT(
      plain("hhh\n"),
      LIST(TYPE -> UNORDERED, LEVEL -> 1,
        ITEM(plain("a")),
        LIST(TYPE -> UNORDERED, LEVEL -> 2, ITEM(plain("b"))),
        ITEM(plain("c"))),
      plain("next"))
  }

  it should "create an empty sublist if one level is omitted" in {
    "hhh\n  *a\n***b\n*c\n\nnext" parseTo ROOT(
      plain("hhh\n"),
      LIST(TYPE -> UNORDERED, LEVEL -> 1,
        ITEM(plain("a")),
        LIST(TYPE -> UNORDERED, LEVEL -> 2,
          LIST(TYPE -> UNORDERED, LEVEL -> 3, ITEM(plain("b")))),
        ITEM(plain("c"))),
      plain("next"))
  }

  it should "accept a sublist of #" in {
    "hhh\n  *a\n##b\n*c\n\nnext" parseTo ROOT(
      plain("hhh\n"),
      LIST(TYPE -> UNORDERED, LEVEL -> 1,
        ITEM(plain("a")),
        LIST(TYPE -> ORDERED, LEVEL -> 2, ITEM(plain("b"))),
        ITEM(plain("c"))),
      plain("next"))
  }

  it should "end when a list of # begins" in {
    "*a\n#b\n" parseTo ROOT(
      LIST(TYPE -> UNORDERED, LEVEL -> 1, ITEM(plain("a"))),
      LIST(TYPE -> ORDERED, LEVEL -> 1, ITEM(plain("b"))))
  }

  it should "not keep state between to executions" in {
    "#a" parseTo LIST(LEVEL -> 1, TYPE -> ORDERED, ITEM(plain("a")))
    "#a" parseTo LIST(LEVEL -> 1, TYPE -> ORDERED, ITEM(plain("a")))
  }

  behavior of "|"

  it should "parse as table" in {
    "a|b" parseTo plain("a|b")
  }

  it should "accept headings, formats inside, a caption" in {
    "|=h1|=h2|\n   |a|b| \n |c \n |**d**e|f|g \n!<nonfloat>This table show interesting \"data\"\nnext" parseTo ROOT(
      TABLE(COLUMNS -> 3, ROWS -> 4, FLOAT -> false,
        Attribute("1,1") -> TABLE_CELL(HEADER -> true, plain("h1")),
        Attribute("1,2") -> TABLE_CELL(HEADER -> true, plain("h2")),
        Attribute("2,1") -> TABLE_CELL(plain("a")),
        Attribute("2,2") -> TABLE_CELL(plain("b")),
        Attribute("3,1") -> TABLE_CELL(plain("c")),
        Attribute("4,1") -> TABLE_CELL(BOLD(plain("d")), plain("e")),
        Attribute("4,2") -> TABLE_CELL(plain("f")),
        Attribute("4,3") -> TABLE_CELL(plain("g")),
        CAPTION -> ROOT(plain("This table show interesting \"data\""))),
      plain("next"))
  }


  it should "allow customizers for colspan, width, align" in {
    "|<colspan=2>a|<width=5cm><align-cell=right><align=left>b|<width=6pt><colspan=2>|" parseTo TABLE(
      COLUMNS -> 5, ROWS -> 1, FLOAT -> true, WIDTH(3) -> "5cm", WIDTH(4) -> "6pt", ALIGN(3) -> LEFT,
      Attribute("1,1") -> TABLE_CELL(SPAN -> 2, plain("a")),
      Attribute("1,3") -> TABLE_CELL(ALIGN -> RIGHT, plain("b")),
      Attribute("1,4") -> TABLE_CELL(SPAN -> 2))
  }

  behavior of "\\"

  it should "be parsed as a newline" in {
    "a\\\\b" parseTo ROOT(plain("a"), NEWLINE(), plain("b"))
  }

  it should "ignore all whitespaces before it" in {
    "a  \n \r \\\\b" parseTo ROOT(plain("a"), NEWLINE(), plain("b"))
  }

  behavior of "----"

  it should "be plain text if shorter than 4" in {
    "---bla" parseTo plain("---bla")
  }

  it should "be parsed as a line if at least 4 length" in {
    "----bla" parseTo ROOT(LINE(), plain("bla"))
  }

  behavior of "<,>,-,= combined to arrows"

  it should "parse as arrows" in {
    "a-->b==>c<--d<==e<-->f<==>ggg->x=>x<-x<=x<->x<=>x" parseTo ROOT(
      plain("a"), symbol("-->", ARROW_RIGHT),
      plain("b"), symbol("==>", DOUBLE_ARROW_RIGHT),
      plain("c"), symbol("<--", ARROW_LEFT),
      plain("d"), symbol("<==", DOUBLE_ARROW_LEFT),
      plain("e"), symbol("<-->", ARROW_BOTH),
      plain("f"), symbol("<==>", DOUBLE_ARROW_BOTH),
      plain("ggg->x=>x<-x<=x<->x<=>x"))
  }

  behavior of "unclosed formats"

  it should "be parsed as plain text" in {
    "**hhh" parseTo BOLD(plain("hhh"))
  }
}
