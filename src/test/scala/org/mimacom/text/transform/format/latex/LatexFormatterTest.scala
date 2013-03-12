package org.mimacom.text.transform.format.latex

import org.scalatest.FlatSpec
import org.mimacom.text.transform.format.ImageLoader
import org.mimacom.text.transform.{Attribute, AttributeValue, Segment}
import org.mimacom.text.transform.Segment._
import org.mimacom.text.transform.Attribute._
import org.mimacom.text.transform.AttributeValue._
import org.mimacom.text.transform.Name._
import java.util.Locale


/**
 *
 */
class LatexFormatterTest extends FlatSpec {
  val imageLoader = new ImageLoader {
    def loadImage(source: Segment, name: String) = if (name == "nix") None else Some("load:" + name)
  }

  val formatter = new LatexFormatter(2, Locale.GERMAN, imageLoader)


  private def assertFormat(expected: String, segments: Segment*) {
    val value = formatter.format(Segment(ROOT, segments: _*))
    assert(expected === value)
  }

  behavior of "basic formatting"

  it should "simply replace formats" in {
    assertFormat("hallo", plainText("hallo"))
    assertFormat("\\\\ \\hline \\noindent \\\\", Segment(LINE))
    assertFormat("\\textbf{hallo}", Segment(BOLD, plainText("hallo")))
    assertFormat("\\textit{hallo}", Segment(ITALICS, plainText("hallo")))
  }

  it should "support mixing with plain text" in {
    assertFormat("a\\textbf{hallo}b",
      plainText("a"),
      Segment(BOLD, plainText("hallo")),
      plainText("b"))
  }

  it should "support nesting" in {
    assertFormat("a\\textbf{c\\textit{d}e}b",
      plainText("a"),
      Segment(BOLD, plainText("c"), Segment(ITALICS, plainText("d")), plainText("e")),
      plainText("b"))
  }

  behavior of "list"

  it should "translate unordered list into an itemize environment" in {
    assertFormat("\\begin{itemize}\\item{a}\\item{b}\\end{itemize}",
      Segment(LIST,
        TYPE -> UNORDERED,
        Segment(LIST_ITEM, plainText("a")),
        Segment(LIST_ITEM, plainText("b"))))
  }

  it should "translate unordered list into an enumerate environment" in {
    assertFormat("\\begin{enumerate}\\item{a}\\item{b}\\end{enumerate}",
      Segment(LIST,
        TYPE -> ORDERED,
        Segment(LIST_ITEM, plainText("a")),
        Segment(LIST_ITEM, plainText("b"))))
  }

  behavior of "link"

  it should "translate external link into href and support nested format" in {
    assertFormat("a\\href{http://}{d1\\textbf{d2}}b",
      plainText("a"),
      Segment(LINK, TARGET -> "http://", plainText("d1"), Segment(BOLD, plainText("d2"))),
      plainText("b"))
  }

  it should "translate link to image into ref with 'Abbildung'" in {
    assertFormat("aAbbildung \\ref{image:img}b",
      plainText("a"),
      Segment(LINK, TARGET -> "image:img", plainText("d1"), Segment(BOLD, plainText("d2"))),
      plainText("b"))
  }

  it should "translate other links into plain ref" in {
    assertFormat("a\\ref{other}b",
      plainText("a"),
      Segment(LINK, TARGET -> "other", plainText("d1"), Segment(BOLD, plainText("d2"))),
      plainText("b"))
  }

  behavior of "heading"

  it should "translate into the correct element of section/paragraph and not support nested format but correct escaping" in {
    assertFormat("\\section{**hallo**}", Segment(HEADING, plainText("**hallo**")))
    assertFormat("\\subsection{**hallo**}", Segment(HEADING, LEVEL -> 1, plainText("**hallo**")))
    assertFormat("\\subparagraph{**hallo**}", Segment(HEADING, LEVEL -> 10000, plainText("**hallo**")))
    assertFormat("\\subsubsection{Widget: \"`achtung\"'}", Segment(HEADING, LEVEL -> 2, plainText("Widget: \"achtung\"")))
  }

  behavior of "special characters"

  it should "escape correctly" in {
    assertFormat("\n \\textbackslash  \\# \\$ \\% \\& \\_ \\{ \\} {[} ] \\~{} \\^{} \"`xxx\"' „xxx\"'",
      plainText("\n \\ # $ % & _ { } [ ] ~ ^ \"xxx\" „xxx\""))
  }

  behavior of "symbols"

  it should "translate into corresponding latex symbols" in {
    def symbol(typ: AttributeValue) = Segment.symbol("", typ)

    assertFormat("$\\leftarrow$ $\\rightarrow$ $\\Leftarrow$ $\\Rightarrow$ $\\leftrightarrow$ $\\Leftrightarrow$ ",
      symbol(ARROW_LEFT), symbol(ARROW_RIGHT),
      symbol(DOUBLE_ARROW_LEFT), symbol(DOUBLE_ARROW_RIGHT),
      symbol(ARROW_BOTH), symbol(DOUBLE_ARROW_BOTH))
  }

  behavior of "image"

  val start = "\\begin{figure}[hpt]\n\\centering\n\\includegraphics["
  val end = "]{load:target}\n\\caption{bild} \\label{image:bild}\n\\end{figure}"
  val image = Segment(IMAGE, TARGET -> "target", FLOAT -> true, plainText("bild"))

  it should "translate into figure environment if floating and have textwidth if no width is specified" in {
    assertFormat(start + "width=1.0\\textwidth" + end, image)
  }

  it should "understand absolute width attribute" in {
    assertFormat(start + "width=5cm" + end, image.add(WIDTH -> "5cm"))
  }

  it should "understand relative width attribute" in {
    assertFormat(start + "width=0.55\\textwidth" + end, image.add(WIDTH -> "55%"))
  }

  it should "understand angle attribute and support multiple attributes" in {
    assertFormat(start + "height=0.1\\textheight,angle=45" + end,
      image.add(ANGLE -> "45", HEIGHT -> "10%", WIDTH -> null))
  }

  it should "show an error message it cannot be found" in {
    assertFormat("Bild nix nicht gefunden", Segment(IMAGE, TARGET -> "nix", plainText("bild")))
  }

  it should "make use of captionof if not floating" in {
    val start = "\\begin{center}\\includegraphics["
    val end = "]{load:target}\n\\captionof{figure}{bild} \\label{image:bild}\n\\end{center}"
    val image = Segment(IMAGE, TARGET -> "target", FLOAT -> false, plainText("bild"))
    assertFormat(start + "width=1.0\\textwidth" + end, image)
  }

  behavior of "table"

  val table = Segment(TABLE, ROWS -> 1, COLUMNS -> 2, FLOAT -> true, Attribute("1,1") -> Segment(TABLE_CELL, plainText("a1")))
  val prefix = "\\begin{table}[hpt] \\centering\n\\begin{tabular}"
  val postfix = "\\end{tabular}\\end{table}"

  it should "translate into a tabular environment wrapped into a table environment if floating" in {
    assertFormat(prefix + "{l l }\n" +
      "a1&\\tabularnewline \n" +
      postfix, table)
  }

  it should "understand width attribute" in {
    assertFormat(prefix + "{p{5cm} l }\n" +
      "a1&\\tabularnewline \n" +
      postfix, table.add(WIDTH.index(1) -> "5cm"))
  }

  it should "underline the header" in {
    assertFormat(prefix + "{p{5cm} l }\n" +
      "a1&\\tabularnewline \\hline\n" +
      postfix,
      table.add(Attribute("1,1") -> Segment(TABLE_CELL, HEADER -> true, plainText("a1"))))
  }

  it should "translate align left into raggedright" in {
    assertFormat(prefix + "{p{5cm} l }\n" +
      "\\raggedright a1&\\tabularnewline \n" +
      postfix,
      table.add(Attribute("1,1") -> Segment(TABLE_CELL, ALIGN -> LEFT, plainText("a1"))))
  }

  it should "translate align right into reggedleft" in {
    assertFormat(prefix + "{p{5cm} l }\n" +
      "\\raggedleft a1&\\tabularnewline \n" +
      postfix,
      table.add(Attribute("1,1") -> Segment(TABLE_CELL, ALIGN -> RIGHT, plainText("a1"))))
  }

  it should "translate span into multicolumn" in {
    assertFormat(prefix + "{p{5cm} l }\n" +
      "\\multicolumn{2}{l}{a1}\\tabularnewline \n" +
      postfix,
      table.add(Attribute("1,1") -> Segment(TABLE_CELL, SPAN -> 2, plainText("a1"))))
  }

  it should "translate into a simple tabular environment if not floating" in {
    val table = Segment(TABLE, ROWS -> 1, COLUMNS -> 2, FLOAT -> false,
      CAPTION -> Segment.plainText("new table \"here\""),
      Attribute("1,1") -> Segment(TABLE_CELL, plainText("a1")))
    val prefix = "\\begin{center}\\begin{tabular}"
    val postfix = "\\end{tabular}\n" +
      "\\captionof{table}{new table \"`here\"'} \\label{table:new table \"`here\"'}" +
      "\\end{center}"

    assertFormat(prefix + "{l l }\n" +
      "a1&\\tabularnewline \n" +
      postfix, table)
  }
}
