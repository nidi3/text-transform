package stni.text.transform.parse.html

import stni.text.transform.{Attribute, AttributeValue, Segment}
import stni.text.transform.Name._
import stni.text.transform.Attribute._
import stni.text.transform.AttributeValue._
import stni.text.transform.Segment._

import collection.mutable.ListBuffer
import java.util.regex.Pattern
import org.xml.sax.InputSource
import java.io.StringReader
import xml.parsing.{NoBindingFactoryAdapter, FactoryAdapter}
import xml.factory.XMLLoader
import stni.text.transform.parse.{CustomizerParser, AbstractParser}
import stni.text.transform.TransformContext
import org.apache.commons.lang3.text.translate.AggregateTranslator
import org.apache.commons.lang3.text.translate.LookupTranslator
import org.apache.commons.lang3.text.translate.EntityArrays
import org.apache.commons.lang3.text.translate.NumericEntityUnescaper
import xml.{NodeSeq, Elem, Node, Text}

/**
 *
 */
class HtmlParser(context: TransformContext) extends AbstractParser(context) {
  private val CUSTOMIZER_COLSPAN = "colspan"
  private val CUSTOMIZER_WIDTH = "width"
  private val CUSTOMIZER_ALIGN = "align"
  private val CUSTOMIZER_ALIGN_CELL = "align-cell"

  val LINK_PATTERN = Pattern.compile("(https?://[^\\Q (,.?!:;\"')\\E]*)")
  val UNESCAPE_HTML4 = new AggregateTranslator(
    lookupTranslator(EntityArrays.ISO8859_1_UNESCAPE().asInstanceOf[Array[Array[CharSequence]]]),
    lookupTranslator(EntityArrays.HTML40_EXTENDED_UNESCAPE().asInstanceOf[Array[Array[CharSequence]]]),
    new NumericEntityUnescaper()
  )

  def lookupTranslator(a: Array[Array[CharSequence]]) = new LookupTranslator(a: _*)

  object EntityIgnoringXml extends XMLLoader[Elem] {
    override def adapter: FactoryAdapter = new NoBindingFactoryAdapter {
      override def resolveEntity(publicId: String, systemId: String): InputSource =
        new InputSource(new StringReader(""))
    }
  }

  override def parseImpl(): Segment = {
    val unescaped = UNESCAPE_HTML4.translate(input)
    val xml = EntityIgnoringXml.loadString( s"""<!DOCTYPE a PUBLIC "bla" "blu"><root $nsDefs>$unescaped</root>""")
    cleanNewlines(parse(xml, 1)(0))
  }

  def trimNewlines(seg: Segment) = {
    val ch = seg.children
    while (!ch.isEmpty && ch(0).name == NEWLINE) ch.remove(0)
    while (!ch.isEmpty && ch(ch.length - 1).name == NEWLINE) ch.remove(ch.length - 1)
    seg
  }

  def cleanNewlines(seg: Segment): Segment = {
    trimNewlines(seg)

    val ch = seg.children
    var i = 0
    while (i < ch.length) {
      cleanNewlines(ch(i))
      if (List(HEADING, TABLE, IMAGE, LIST) contains ch(i).name) {
        if (i > 0 && ch(i - 1).name == NEWLINE) {
          ch.remove(i - 1)
          i -= 2
        } else if (i < ch.length - 1 && ch(i + 1).name == NEWLINE) {
          ch.remove(i + 1)
          i -= 1
        }
      }
      i += 1
    }
    seg
  }


  def nsDefs = namespaces.map(ns => s"""xmlns:$ns="$ns" """).mkString

  def namespaces: Seq[String] = Nil

  def parse(xml: Seq[Node], listLevel: Int): Seq[Segment] =
    xml.flatMap(n => parse(n, listLevel))

  def parse(node: Node, listLevel: Int): Seq[Segment] = {
    def heading(ns: NodeSeq, headingLevel: Int) = HEADING(parse(ns, listLevel): _*)(LEVEL -> (headingLevel + context.headingLevel))

    node match {
      case <root>{ns@_*}</root> => List(ROOT(parse(ns, listLevel): _*))
      case <strong>{ns@_*}</strong> if (!ns.isEmpty) => List(BOLD(parse(ns, listLevel): _*))
      case <p>{ns@_*}</p> if (!ns.isEmpty) => parse(ns, listLevel) ++ List(NEWLINE())
      case <span>{ns@_*}</span> if (!ns.isEmpty) => parse(ns, listLevel)
      case <div>{ns@_*}</div> if (!ns.isEmpty) => parse(ns, listLevel)
      case <br/> => List(NEWLINE())
      case <hr/> => List(LINE())
      case <em>{ns@_*}</em> if (!ns.isEmpty) => List(ITALICS(parse(ns, listLevel): _*))
      case <h1>{ns@_*}</h1> if (!ns.isEmpty) => List(heading(ns, 1))
      case <h2>{ns@_*}</h2> if (!ns.isEmpty) => List(heading(ns, 2))
      case <h3>{ns@_*}</h3> if (!ns.isEmpty) => List(heading(ns, 3))
      case <h4>{ns@_*}</h4> if (!ns.isEmpty) => List(heading(ns, 4))
      case <h5>{ns@_*}</h5> if (!ns.isEmpty) => List(heading(ns, 5))
      case <h6>{ns@_*}</h6> if (!ns.isEmpty) => List(heading(ns, 6))
      case <ol>{ns@_*}</ol> if (!ns.isEmpty) => List(LIST(parse(ns, listLevel + 1): _*)(TYPE -> AttributeValue.ORDERED, LEVEL -> listLevel))
      case <ul>{ns@_*}</ul> if (!ns.isEmpty) => List(LIST(parse(ns, listLevel + 1): _*)(TYPE -> AttributeValue.UNORDERED, LEVEL -> listLevel))
      case <li>{ns@_*}</li> if (!ns.isEmpty) => List(ITEM(parse(ns, listLevel): _*))
      case <table>{ns@_*}</table> => List(table(ns, listLevel))
      case n@ <a>{ns@_*}</a> =>
        val href = (n \ "@href").text
        val desc = if (ns.isEmpty) List(plain(href)) else parse(ns, listLevel)
        List(LINK(desc: _*)(TARGET -> href, TYPE -> URL))
      case Text(t) =>
        val list = new ListBuffer[Segment]
        val s = new StringBuffer
        val m = LINK_PATTERN.matcher(t)
        while (m.find) {
          m.appendReplacement(s, "")
          list += plain(s.toString)
          list += LINK(plain(m.group(1)), TARGET -> m.group(1), TYPE -> URL)
          s.setLength(0)
        }
        m.appendTail(s)
        list += plain(s.toString)
        list
      case _ => Nil
    }
  }

  private def table(ns: NodeSeq, listLevel: Int): Segment = {
    val table = TABLE()
    var colIndex = 0

    def cell(col: Node, index: Int) = {
      val cell = TABLE_CELL()
      val content = parse(col.child, listLevel)
      if (!content.isEmpty && content(0).name == PLAIN) {
        content(0)(TEXT -> CustomizerParser(content(0)(TEXT).get.asInstanceOf[String], (name, value) => name match {
          case CUSTOMIZER_WIDTH => table(WIDTH(index) -> value)
          case CUSTOMIZER_ALIGN => table(ALIGN(index)-> leftOrRight(value))
          case CUSTOMIZER_ALIGN_CELL => cell(ALIGN -> leftOrRight(value))
          case _ =>
        }).trim)
      }
      try {
        val span = Integer.parseInt((col \ "@colspan").text)
        if (span > 1) cell(SPAN -> span)
        colIndex += span - 1
      } catch {
        case e: NumberFormatException =>
      }
      if (col.label == "th" || (col \ "@class").text == "highlight") cell(HEADER -> true)
      trimNewlines(cell(content: _*))
    }

    var rowIndex = 1
    var maxColumns = 1
    for (row <- ns \ "tr") {
      colIndex = 1
      for (col <- row.child) {
        if (col.label == "td" || col.label == "th") {
          table(Attribute(rowIndex + "," + colIndex) -> cell(col, colIndex))
          colIndex += 1
        }
      }
      maxColumns = Math.max(maxColumns, colIndex)
      rowIndex += 1
    }
    table(ROWS -> (rowIndex - 1), COLUMNS -> (maxColumns - 1))
    table
  }


}