package org.mimacom.text.transform.parse.wiki.creole

import org.mimacom.text.transform.{AttributeValue, Segment}
import org.mimacom.text.transform.AttributeValue._
import org.mimacom.text.transform.Name._
import org.mimacom.text.transform.Attribute._
import org.mimacom.text.transform.parse.wiki.{ListParser, AbstractWikiParser}

/**
 *
 */
class CreoleWikiParser extends AbstractWikiParser {
  private val HTTP = "http:"
  private val HTTPS = "https:"
  private val MINUSES_FOR_LINE = 4

  private val CUSTOMIZER_WIDTH = "width"
  private val CUSTOMIZER_HEIGHT = "height"
  private val CUSTOMIZER_ANGLE = "angle"
  private val CUSTOMIZER_NONFLOAT = "nonfloat"

  var defState: Segment = null

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
    if (text.isEmptyOrNewline) {
      savePos()
      nextChar()
      defState = Segment(DEFINITION)
      val cp = new CustomizerParser(readUntil("\n").dropWhile(_ <= ' '))
      while (cp.find()) {
        cp.name match {
          case CUSTOMIZER_WIDTH => defState.add(WIDTH -> cp.value)
        }
      }
      defState.add(TEXT -> cp.rest)
      if (currentChar == ':') {
        while (currentChar == ':') {
          nextChar()
          defState.add(parseSub(readUntil("\n").dropWhile(_ <= ' ')): _*)
        }
        addToResult(defState)
      } else {
        text.append(restorePos())
      }
      defState = null
    } else {
      text.append(";")
      nextChar()
    }
  }

  private def newline() {
    if (nextChar() == '\\') {
      nextChar()
      while (currentChar <= ' ') {
        nextChar()
      }
      text.trim()
      addToResult(Segment(NEWLINE))
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
        addToResult(Segment(BOLD, parseSub(bold): _*))
      }
    }
  }

  private def italics() {
    if (nextChar() == '/') {
      text.endsWith(HTTP, HTTPS) match {
        case Some(end) =>
          text.removeLast(end.length)
          val link = end + "/" + readUntilChar("(,.?!:;\\\"') ")
          addToResult(Segment(LINK, TARGET -> link, Segment.plainText(link)))
        case None =>
          nextChar()
          val italics = readUntil("//")
          if (italics.length > 0) {
            addToResult(Segment(ITALICS, parseSub(italics): _*))
          }
      }
    } else {
      text.append('/')
    }
  }

  private def link() {
    readUntilClose('[', "]]").map(data => {
      splitTarget(data, Segment(LINK))
    })
  }

  private def image() {
    readUntilClose('{', "}}").map(data => {
      val segment = Segment(IMAGE, FLOAT -> true)
      val cp = new CustomizerParser(data)
      while (cp.find()) {
        cp.name match {
          case CUSTOMIZER_WIDTH => segment.add(WIDTH -> cp.value)
          case CUSTOMIZER_HEIGHT => segment.add(HEIGHT -> cp.value)
          case CUSTOMIZER_ANGLE => segment.add(ANGLE -> cp.value)
          case CUSTOMIZER_NONFLOAT => segment.add(FLOAT -> false)
        }
      }
      splitTarget(cp.rest, segment)
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
    segment.add(parseSub(desc): _*)
    segment.add(TARGET -> target)
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
      val level = getCount('=') + initLevel
      val heading = readUntil("=" * level, "\n")
      while (currentChar <= ' ') {
        nextChar()
      }
      if (heading.length > 0) {
        addToResult(Segment(HEADING, LEVEL -> level, Segment.plainText(heading)))
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
      addToResult(Segment(LINE))
    } else {
      text.append("-" * count)
    }
  }
}
