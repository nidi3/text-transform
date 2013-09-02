package stni.text.transform.parse.wiki.creole

import stni.text.transform.{AttributeValue, Segment}
import stni.text.transform.AttributeValue._
import stni.text.transform.Name._
import stni.text.transform.Segment._
import stni.text.transform.Attribute._
import stni.text.transform.TransformContext
import stni.text.transform.parse.wiki.{ListParser, AbstractWikiParser}
import stni.text.transform.parse.TagCustomizerParser

/**
 *
 */
class CreoleWikiParser(context: TransformContext) extends AbstractWikiParser(context) {
  private val HTTP = "http:"
  private val HTTPS = "https:"
  private val IMAGE_PREFIX = "image:"
  private val DOCUMENT_PREFIX = "document:"

  private val MINUSES_FOR_LINE = 4

  private val CUSTOMIZER_WIDTH = "width"
  private val CUSTOMIZER_HEIGHT = "height"
  private val CUSTOMIZER_ANGLE = "angle"
  private val CUSTOMIZER_NONFLOAT = "nonfloat"

  protected def handleSpecialChar() {
    currentChar match {
      case '*' => boldOrList()
      case '/' => italics()
      case '=' => headingOrArrow()
      case '[' => link()
      case '{' => image()
      case '#' =>
        nextChar()
        list('#', ORDERED)
      case '|' => table()
      case '-' => lineOrArrow()
      case '<' => arrow()
      case '\\' => newline()
      case ';' => definition()
      case _ =>
        text.append(currentChar)
        nextChar()
    }
  }

  private def definition() {
    def readLine() = readUntil("\n").dropWhile(_ <= ' ')

    if (text.isEmptyOrNewline) {
      nextChar()
      val definition = DEFINITION()
      definition(TEXT ->
        TagCustomizerParser(readLine(), (name, value) => name match {
          case CUSTOMIZER_WIDTH => definition(WIDTH -> value)
        })
      )
      while (currentChar == ':') {
        nextChar()
        definition(ITEM(parseSub(readLine()).children: _*))
      }
      addToResult(definition)
    } else {
      text.append(";")
      nextChar()
    }
  }

  private def newline() {
    if (nextChar() == '\\') {
      nextChar()
      skipWhitspaces()
      text.trim()
      addToResult(NEWLINE())
    } else {
      text.append('\\')
    }
  }

  private def boldOrList() {
    if (nextChar != '*' || listState.isInList) {
      list('*', UNORDERED)
    } else {
      nextChar()
      val bold = readUntil("**")
      if (bold.length > 0) {
        addToResult(BOLD(parseSub(bold).children: _*))
      }
    }
  }

  private def splitLinkSuffix(link: String) = {
    if (!"(,.?!:;\\\"')".contains(link(link.length - 1))) (link, "")
    else {
      var pos = link.length - 1
      while ("(,.?!:;\\\"')".contains(link(pos))) pos -= 1
      (link.substring(0, pos+1), link.substring(pos + 1))
    }
  }

  private def italics() {
    if (nextChar() == '/') {
      text.endsWith(HTTP, HTTPS) match {
        case Some(end) =>
          text.removeLast(end.length)
          val link = splitLinkSuffix(end + "/" + readUntilChar(" "))
          addToResult(LINK(TYPE -> URL, TARGET -> link._1, plain(link._1)))
          pushBack(link._2.length)
        case None =>
          nextChar()
          val italics = readUntil("//")
          if (italics.length > 0) {
            addToResult(ITALICS(parseSub(italics).children: _*))
          }
      }
    } else {
      text.append('/')
    }
  }

  private def link() {
    readUntilClose('[', "]]").map(data => {
      if (data.startsWith(IMAGE_PREFIX)) {
        splitTarget(data.substring(IMAGE_PREFIX.length), LINK(TYPE -> IMAGE_REF))
      } else if (data.startsWith(DOCUMENT_PREFIX)) {
        splitTarget(data.substring(DOCUMENT_PREFIX.length), LINK(TYPE -> DOCUMENT_REF))
      } else {
        splitTarget(data, LINK(TYPE -> URL))
      }
    })
  }

  private def image() {
    readUntilClose('{', "}}").map(data => {
      val image = IMAGE(FLOAT -> true)
      splitTarget(
        TagCustomizerParser(data, (name, value) => name match {
          case CUSTOMIZER_WIDTH => image(WIDTH -> value)
          case CUSTOMIZER_HEIGHT => image(HEIGHT -> value)
          case CUSTOMIZER_ANGLE => image(ANGLE -> value)
          case CUSTOMIZER_NONFLOAT => image(FLOAT -> false)
          case _ => //TODO warn
        }),
        image)
    })
  }

  private def readUntilClose(open: Char, close: String): Option[String] = {
    if (nextChar() != open) {
      text.append(open)
      None
    } else {
      nextChar()
      Some(readUntil(close))
    }
  }

  private def splitTarget(data: String, segment: Segment) {
    val pos = data.indexOf('|')
    val target = data.substring(0, if (pos < 0) data.length else pos)
    val desc = data.substring(pos + 1)
    segment(parseSub(desc).children: _*)(TARGET -> target)
    addToResult(segment)
  }

  private def headingOrArrow() {
    if (nextChar != '=') {
      heading(0)
    } else {
      if (nextChar != '>') {
        heading(1)
      } else {
        pushBack(2)
        arrow()
      }
    }
  }

  private def heading(initLevel: Int) {
    if (!text.isEmptyOrNewline) {
      text.append('=')
    } else {
      val level = getCount('=') + initLevel + context.headingLevel
      val heading = readUntil("=" * level, "\n").trim
      skipWhitspaces()
      if (heading.length > 0) {
        addToResult(HEADING(LEVEL -> level, plain(heading)))
      }
    }
  }

  private def list(sym: Char, listType: AttributeValue) {
    if (text.isEmptyOrNewline) {
      new ListParser(this, listState, listType).list(sym)
    } else {
      text.append(sym)
    }
  }

  private def table() {
    if (text.isEmptyOrNewline) {
      new TableParser(this).parseTable()
    } else {
      text.append('|')
      nextChar()
    }
  }

  private def lineOrArrow() {
    if (nextChar != '-') {
      line(0)
    } else {
      if (nextChar != '>') {
        line(1)
      } else {
        pushBack(2)
        arrow()
      }
    }
  }

  private def line(initCount: Int) {
    val count = getCount('-') + initCount
    if (count >= MINUSES_FOR_LINE) {
      addToResult(LINE())
    } else {
      text.append("-" * count)
    }
  }
}
