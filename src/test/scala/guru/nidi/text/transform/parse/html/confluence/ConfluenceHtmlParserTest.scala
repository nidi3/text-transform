package guru.nidi.text.transform.parse.html.confluence

import guru.nidi.text.transform.Name._
import guru.nidi.text.transform.Attribute._
import guru.nidi.text.transform.Segment._
import guru.nidi.text.transform.AttributeValue._
import guru.nidi.text.transform.parse.ParserTest
import guru.nidi.text.transform.{TransformContext, Segment, ResourceLoader}
import java.util.Locale


/**
 *
 */
class ConfluenceHtmlParserTest extends ParserTest {
  val parser = new ConfluenceHtmlParser(new TransformContext(0, Locale.GERMAN, new ResourceLoader {
    def loadResource(source: Segment, name: String): Option[String] = Some("<strong>" + name + "</strong>")
  }))

  behavior of "<ac:link>"

  it should "understand file attachments" in {
    """<ac:link><ri:attachment ri:filename="file" /></ac:link>""" parseTo
      LINK(TARGET -> "file", TYPE -> FILE_REF, plain("file"))
  }

  it should "understand file attachments with custom name" in {
    """<ac:link><ri:attachment ri:filename="file" /><ac:plain-text-link-body><![CDATA[name]]></ac:plain-text-link-body></ac:link>""" parseTo
      LINK(TARGET -> "file", TYPE -> FILE_REF, plain("name"))
  }

  it should "parse page references without content-title" in {
    """<ac:link><ri:page /><ac:plain-text-link-body><![CDATA[link]]></ac:plain-text-link-body></ac:link>""" parseTo
      LINK(TARGET -> ":", TYPE -> DOCUMENT_REF, plain("link"), SUB -> ROOT(BOLD(plain(":"))))
  }

  it should "parse page references without space-key" in {
    """<ac:link><ri:page ri:content-title="Home" /><ac:plain-text-link-body><![CDATA[link]]></ac:plain-text-link-body></ac:link>""" parseTo
      LINK(TARGET -> ":Home", TYPE -> DOCUMENT_REF, plain("link"), SUB -> ROOT(BOLD(plain(":Home"))))
  }

  it should "parse page references with space-key" in {
    """<ac:link><ri:page ri:content-title="Home" ri:space-key="Space" /><ac:plain-text-link-body><![CDATA[link]]></ac:plain-text-link-body></ac:link>""" parseTo
      LINK(TARGET -> "Space:Home", TYPE -> DOCUMENT_REF, plain("link"), SUB -> ROOT(BOLD(plain("Space:Home"))))
  }

  it should "adjust heading level in referenced page" in {
    """<ac:link><ri:page ri:content-title="&lt;h1>h&lt;/h1>" /><ac:plain-text-link-body><![CDATA[link]]></ac:plain-text-link-body></ac:link>""" parseTo
      LINK(TARGET -> ":<h1>h</h1>", TYPE -> DOCUMENT_REF, plain("link"), SUB -> ROOT(BOLD(plain(":"), HEADING(plain("h"), LEVEL -> 2))))
  }

  behavior of "<ac:image>"

  it should "be parsed as image" in {
    """<ac:image><ri:attachment ri:filename="Bonsai.gif" /></ac:image>""" parseTo
      IMAGE(TARGET -> "Bonsai.gif")
  }

  behavior of "macro include"

  it should "be parsed as DOCUMENT_INCLUDE" in {
    """<ac:macro ac:name="include"><ac:default-parameter>space:page</ac:default-parameter></ac:macro>""" parseTo
      LINK(TYPE -> DOCUMENT_INCLUDE, TARGET -> "space:page", BOLD(plain("space:page")))
  }
}
