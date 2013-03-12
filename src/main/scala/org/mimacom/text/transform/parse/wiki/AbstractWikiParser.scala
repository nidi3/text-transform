package org.mimacom.text.transform.parse.wiki


import org.mimacom.text.transform.parse.{TextAccumulator, AbstractParser}
import org.mimacom.text.transform.Segment
import org.mimacom.text.transform.AttributeValue._


/**
 *
 */
abstract class AbstractWikiParser extends AbstractParser {
  val HTTP_SLASH = "http://"
  val HTTPS_SLASH = "https://"

  val listState = new ListState
  protected val text = new TextAccumulator

  protected def handleSpecialChar()

  override def doParse() {
    while (currentChar != AbstractParser.EOI) {
      val resultsBefore = result.size
      handleSpecialChar()
      if (resultsBefore != result.size) {
        addPlainTextAt(resultsBefore)
      }
    }
    addPlainTextAtEnd()
  }

  override protected def reset() {
    super.reset()
    listState.reset()
    text.reset()
  }

  protected def addPlainTextAtEnd() {
    addPlainTextAt(result.size)
  }

  protected def addPlainTextAt(resultPos: Int) {
    if (!text.toString.isEmpty) {
      addToResult(resultPos, Segment.plainText(text.toString))
      text.reset()
    }
  }

  protected def arrow() {
    currentChar match {
      case '<' => arrowLeft()
      case '=' => arrowRight('=')
      case '-' => arrowRight('-')
      case _ =>
    }
  }

  private def arrowRight(first: Char) {
    if (nextChar() != first) {
      text.append(first)
    } else {
      if (nextChar() != '>') {
        text.append(first)
        text.append(first)
      } else {
        nextChar()
        if (first == '-') {
          addToResult(Segment.symbol("-->", ARROW_RIGHT))
        } else {
          addToResult(Segment.symbol("==>", DOUBLE_ARROW_RIGHT))
        }
      }
    }
  }

  private def arrowLeft(){
    nextChar() match {
      case '-' =>
        if (nextChar() != '-') {
          text.append("<-")
        } else {
          if (nextChar() == '>') {
            nextChar()
            addToResult(Segment.symbol("<-->", ARROW_BOTH))
          } else {
            addToResult(Segment.symbol("<--", ARROW_LEFT))
          }
        }
      case '=' =>
        if (nextChar() != '=') {
          text.append("<=")
        } else {
          if (nextChar() == '>') {
            nextChar()
            addToResult(Segment.symbol("<==>", DOUBLE_ARROW_BOTH))
          } else {
            addToResult(Segment.symbol("<==", DOUBLE_ARROW_LEFT))
          }
        }
      case _ =>
        text.append('<')
    }
  }
}
