package stni.text.transform.parse.html.confluence

import stni.text.transform.parse.html.HtmlParser
import xml.Node
import stni.text.transform.Segment
import stni.text.transform.Name._
import stni.text.transform.Segment._
import stni.text.transform.Attribute._
import stni.text.transform.AttributeValue._

/**
 *
 */
class ConfluenceHtmlParser extends HtmlParser {
  override def namespaces = List("ac","ri")

  override def parse(node: Node, listLevel: Int): Seq[Segment] = {
    node match {
      case n @ <ac:link>{ns @ _*}</ac:link> =>
        ns match{
          case List(n @ <ri:attachment>{ns @ _*}</ri:attachment>) =>
            val filename = n \ "@{ri}filename"
            if (filename.isEmpty) Nil
            else List(LINK(TYPE -> FILE, TARGET -> filename(0).text, plain(filename(0).text)))
          case List() => Nil

          case _ => Nil
        }

      case _ => super.parse(node, listLevel)
    }
  }
}

//<p>test</p><p><strong>fett</strong></p><p><em><strong>ita</strong></em></p><p><u><em><strong>und</strong></em></u></p><p style="text-align: right;"><span style="color: rgb(255,0,0);">red</span></p><h1><span style="color: rgb(0,0,0);">h1</span></h1><h2><span style="color: rgb(0,0,0);">h2</span></h2><p><span style="color: rgb(0,0,0);"><br /></span></p><ul><li>bu</li><li>ddd</li></ul><ol><li>num</li><li>num2</li></ol><div><a href="http://google.com">http://google.com</a></div><div>
// <ac:link /></div><div>{{fjkhfksjsdh|<ac:link><ri:attachment ri:filename="Bonsai.gif" /></ac:link>}}</div><div>
// <ac:link><ri:page ri:content-title="Home" ri:space-key="start" /><ac:plain-text-link-body><![CDATA[link]]></ac:plain-text-link-body></ac:link>
// </div><div><ac:image><ri:attachment ri:filename="Bonsai.gif" /></ac:image></div><div>hjhjgjhghjjg</div><div><p><code>xxx=ddd</code></p><p><code>test</code></p><p><code>cheese</code></p><p><code>code</code></p><p><ac:macro ac:name="loremipsum"><ac:default-parameter>1</ac:default-parameter></ac:macro></p><p>&theta;</p><p><ac:emoticon ac:name="laugh" /></p><ac:macro ac:name="html"><ac:plain-text-body><![CDATA[width=5cm]]></ac:plain-text-body></ac:macro><table><tbody><tr><th>head1</th><th>&nbsp;</th><th>&lt;width=10cm</th></tr><tr><td>&nbsp;</td><td>2,2</td><td>&nbsp;</td></tr><tr><td colspan="2">&nbsp;</td><td>3,3</td></tr></tbody></table><p>*<strong>bold</strong>*</p><p>&nbsp;</p></div><p><ac:macro ac:name="children" /></p>