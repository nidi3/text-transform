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
package guru.nidi.text.transform.parse.html

import java.io.StringReader
import java.util.regex.Pattern

import guru.nidi.text.transform.Attribute._
import guru.nidi.text.transform.AttributeValue._
import guru.nidi.text.transform.Name._
import guru.nidi.text.transform.Segment._
import guru.nidi.text.transform.parse.AbstractParser
import guru.nidi.text.transform.{AttributeValue, Segment, TransformContext}
import org.apache.commons.lang3.text.translate._
import org.xml.sax.InputSource

import scala.collection.mutable.ListBuffer
import scala.xml.factory.XMLLoader
import scala.xml.parsing.{FactoryAdapter, NoBindingFactoryAdapter}
import scala.xml.{Elem, Node, NodeSeq, Text}

/**
 *
 */
class HtmlParser(context: TransformContext) extends AbstractParser(context) {
  val PATTERN = Pattern.compile("(https?://[^\\Q (,.?!:;\"')\\E]*)|(<->)|(<=>)|(->)|(<-)|(=>)|(<=)")
  val UNESCAPE_HTML4 = new AggregateTranslator(
    translator(EntityArrays.ISO8859_1_UNESCAPE()),
    translator(EntityArrays.HTML40_EXTENDED_UNESCAPE()),
    new NumericEntityUnescaper()
  )

  def translator(a: Array[Array[String]]): CharSequenceTranslator =
    lookupTranslator(a.asInstanceOf[Array[Array[CharSequence]]])

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
      case <strong>{ns@_*}</strong> if !ns.isEmpty => List(BOLD(parse(ns, listLevel): _*))
      case <b>{ns@_*}</b> if !ns.isEmpty => List(BOLD(parse(ns, listLevel): _*))
      case <em>{ns@_*}</em> if !ns.isEmpty => List(ITALICS(parse(ns, listLevel): _*))
      case <i>{ns@_*}</i> if !ns.isEmpty => List(ITALICS(parse(ns, listLevel): _*))
      case <u>{ns@_*}</u> if !ns.isEmpty => List(UNDERLINED(parse(ns, listLevel): _*))
      case <p>{ns@_*}</p> if !ns.isEmpty => parse(ns, listLevel) ++ List(NEWLINE())
      case <span>{ns@_*}</span> if !ns.isEmpty => parse(ns, listLevel)
      case <div>{ns@_*}</div> if !ns.isEmpty => parse(ns, listLevel)
      case <font>{ns@_*}</font> if !ns.isEmpty => parse(ns, listLevel)
      case <br/> => List(NEWLINE())
      case <hr/> => List(LINE())
      case <h1>{ns@_*}</h1> if !ns.isEmpty => List(heading(ns, 1))
      case <h2>{ns@_*}</h2> if !ns.isEmpty => List(heading(ns, 2))
      case <h3>{ns@_*}</h3> if !ns.isEmpty => List(heading(ns, 3))
      case <h4>{ns@_*}</h4> if !ns.isEmpty => List(heading(ns, 4))
      case <h5>{ns@_*}</h5> if !ns.isEmpty => List(heading(ns, 5))
      case <h6>{ns@_*}</h6> if !ns.isEmpty => List(heading(ns, 6))
      case <ol>{ns@_*}</ol> if !ns.isEmpty => List(LIST(parse(ns, listLevel + 1): _*)(TYPE -> AttributeValue.ORDERED, LEVEL -> listLevel))
      case <ul>{ns@_*}</ul> if !ns.isEmpty => List(LIST(parse(ns, listLevel + 1): _*)(TYPE -> AttributeValue.UNORDERED, LEVEL -> listLevel))
      case <li>{ns@_*}</li> if !ns.isEmpty => List(ITEM(parse(ns, listLevel): _*))
      case n@ <table>{ns@_*}</table> => List(new TableParser(this).parse((n\"@class").text, ns, listLevel))
      case n@ <img>{ns@_*}</img> => List(image((n \ "@src").text,(n \ "@alt").text,(n \ "@id").text))
      case n@ <a>{ns@_*}</a> => List(link((n \ "@href").text,ns,listLevel))
      case Text(t) => text(t)
      case _ => Nil
    }
  }

  private def link(href: String, ns: NodeSeq, listLevel: Int) = {
    val desc = if (ns.isEmpty) List(plain(href)) else parse(ns, listLevel)
    val link = LINK(CAPTION -> ROOT(desc: _*))
    if (href.startsWith("#")) link(TARGET -> href.substring(1), TYPE -> REF)
    else link(TARGET -> href, TYPE -> URL)
  }

  private def image(src: String, alt: String, id: String) = {
    val image = IMAGE(TARGET -> src)
    if (!id.isEmpty) image(ID->id)
    CssParser(alt, (name, value) => name match {
      case "width" if value != null => image(WIDTH -> value)
      case "caption" if value != null => image(CAPTION -> ROOT(plain(value)))
      case _ =>
    })
    image
  }

  private def text(t: String) = {
    val list = new ListBuffer[Segment]
    val s = new StringBuffer
    val m = PATTERN.matcher(t)
    def addPlain() {
      if (s.length() > 0) list += plain(s.toString)
    }

    while (m.find) {
      m.appendReplacement(s, "")
      addPlain()
      val matchedGroup = m.group(0)
      matchedGroup match {
        case "->" => list += symbol("->", ARROW_RIGHT)
        case "=>" => list += symbol("=>", DOUBLE_ARROW_RIGHT)
        case "<-" => list += symbol("<-", ARROW_LEFT)
        case "<=" => list += symbol("<=", DOUBLE_ARROW_LEFT)
        case "<->" => list += symbol("<->", ARROW_BOTH)
        case "<=>" => list += symbol("<=>", DOUBLE_ARROW_BOTH)
        case _ => list += LINK(CAPTION->ROOT(plain(matchedGroup)), TARGET -> matchedGroup, TYPE -> URL)
      }

      s.setLength(0)
    }
    m.appendTail(s)
    addPlain()
    list
  }
}
