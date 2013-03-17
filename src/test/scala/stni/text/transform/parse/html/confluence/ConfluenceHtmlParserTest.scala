package stni.text.transform.parse.html.confluence

import stni.text.transform.Name._
import stni.text.transform.Attribute._
import stni.text.transform.Segment._
import stni.text.transform.AttributeValue._
import stni.text.transform.parse.ParserTest


/**
 *
 */
class ConfluenceHtmlParserTest extends ParserTest {
  val parser = new ConfluenceHtmlParser

  behavior of "<ac:link>"

  it should "understand file attachments" in {
    """<ac:link><ri:attachment ri:filename="file" /></ac:link>""" parseTo
      LINK(TARGET -> "file", TYPE -> FILE, plain("file"))
  }

  it should "understand file attachments with custom name" in {
    """<ac:link><ri:attachment ri:filename="file" /><ac:plain-text-link-body><![CDATA[name]]></ac:plain-text-link-body></ac:link>""" parseTo
      LINK(TARGET -> "file", TYPE -> FILE, plain("file"))
  }
}
