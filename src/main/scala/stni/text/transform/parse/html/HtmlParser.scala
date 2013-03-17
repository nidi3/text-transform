package stni.text.transform.parse.html

import stni.text.transform.{AttributeValue, Segment, Parser}
import stni.text.transform.Name._
import stni.text.transform.Attribute._
import stni.text.transform.AttributeValue._
import stni.text.transform.Segment._
import xml._
import collection.mutable.ListBuffer
import java.util.regex.Pattern
import org.xml.sax.InputSource
import java.io.StringReader
import parsing.{NoBindingFactoryAdapter, FactoryAdapter}
import xml.factory.XMLLoader

/**
 *
 */
class HtmlParser extends Parser {
  val LINK_PATTERN=Pattern.compile("(https?://[^\\Q (,.?!:;\"')\\E]*)")

  object EntityIgnoringXml extends XMLLoader[Elem] {
    override def adapter: FactoryAdapter = new NoBindingFactoryAdapter {
      override def resolveEntity(publicId: String, systemId: String): InputSource =
        new InputSource(new StringReader(""))
    }
  }

  def parse(input: String): Segment = {
    val xml = EntityIgnoringXml.loadString(s"""<!DOCTYPE a PUBLIC "bla" "blu"><root $nsDefs>$input</root>""")
    parse(xml, 1)(0)
  }

  def nsDefs = namespaces.map(ns => s"""xmlns:$ns="$ns" """).mkString

  def namespaces: Seq[String] = Nil

  def parse(xml: Seq[Node], listLevel: Int): Seq[Segment] =
    xml.flatMap(n => parse(n, listLevel))

  def parse(node: Node,listLevel:Int): Seq[Segment] = {
    node match {
      case <root>{ns @ _*}</root> => List(ROOT(parse(ns,listLevel): _*))
      case <strong>{ns @ _*}</strong> if (!ns.isEmpty) => List(BOLD(parse(ns,listLevel): _*))
      case <p>{ns @ _*}</p> if (!ns.isEmpty) => (NEWLINE() +: parse(ns,listLevel)) ++ List(NEWLINE())
      case <span>{ns @ _*}</span> if (!ns.isEmpty) => parse(ns,listLevel)
      case <div>{ns @ _*}</div> if (!ns.isEmpty) => parse(ns,listLevel)
      case <br/> => List(NEWLINE())
      case <hr/> => List(LINE())
      case <em>{ns @ _*}</em> if (!ns.isEmpty) => List(ITALICS(parse(ns,listLevel): _*))
      case <h1>{ns @ _*}</h1> if (!ns.isEmpty) => List(HEADING(parse(ns,listLevel): _*)(LEVEL->1))
      case <h2>{ns @ _*}</h2> if (!ns.isEmpty) => List(HEADING(parse(ns,listLevel): _*)(LEVEL->2))
      case <h3>{ns @ _*}</h3> if (!ns.isEmpty) => List(HEADING(parse(ns,listLevel): _*)(LEVEL->3))
      case <h4>{ns @ _*}</h4> if (!ns.isEmpty) => List(HEADING(parse(ns,listLevel): _*)(LEVEL->4))
      case <h5>{ns @ _*}</h5> if (!ns.isEmpty) => List(HEADING(parse(ns,listLevel): _*)(LEVEL->5))
      case <h6>{ns @ _*}</h6> if (!ns.isEmpty) => List(HEADING(parse(ns,listLevel): _*)(LEVEL->6))
      case <ol>{ns @ _*}</ol> if (!ns.isEmpty) => List(LIST(parse(ns,listLevel+1): _*)(TYPE->AttributeValue.ORDERED,LEVEL->listLevel))
      case <ul>{ns @ _*}</ul> if (!ns.isEmpty) => List(LIST(parse(ns,listLevel+1): _*)(TYPE->AttributeValue.UNORDERED,LEVEL->listLevel))
      case <li>{ns @ _*}</li> if (!ns.isEmpty) => List(ITEM(parse(ns,listLevel): _*))
      case n @ <a>{ns @ _*}</a> =>
        val href = (n \ "@href").text
        val desc = if (ns.isEmpty) List(plain(href)) else parse(ns, listLevel)
        List(LINK(desc: _*)(TARGET -> href, TYPE -> URL))
      case Text(t) =>
        val list = new ListBuffer[Segment]
        val s = new StringBuffer
        val m = LINK_PATTERN.matcher(t)
        while (m.find) {
          m.appendReplacement(s, "")
          list +=plain(s.toString)
          list += LINK(plain(m.group(1)), TARGET -> m.group(1), TYPE -> URL)
          s.setLength(0)
        }
        m.appendTail(s)
        list += plain(s.toString)
        list
      case _ => Nil
    }
  }

}
