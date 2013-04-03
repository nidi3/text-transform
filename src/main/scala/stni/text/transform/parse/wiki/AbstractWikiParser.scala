package stni.text.transform.parse.wiki


import stni.text.transform.parse.{TextAccumulator, AbstractCharReadingParser}
import stni.text.transform.TransformContext
import stni.text.transform.AttributeValue._
import stni.text.transform.Segment._


/**
 *
 */
abstract class AbstractWikiParser(context: TransformContext) extends AbstractCharReadingParser(context) {
  val HTTP_SLASH = "http://"
  val HTTPS_SLASH = "https://"

  val listState = new ListState
  protected val text = new TextAccumulator

  protected def handleSpecialChar()

  override def doParse() {
    while (currentChar != AbstractCharReadingParser.EOI) {
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
      addToResult(resultPos, plain(text.toString))
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
          addToResult(symbol("-->", ARROW_RIGHT))
        } else {
          addToResult(symbol("==>", DOUBLE_ARROW_RIGHT))
        }
      }
    }
  }

  private def arrowLeft() {
    nextChar() match {
      case '-' =>
        if (nextChar() != '-') {
          text.append("<-")
        } else {
          if (nextChar() == '>') {
            nextChar()
            addToResult(symbol("<-->", ARROW_BOTH))
          } else {
            addToResult(symbol("<--", ARROW_LEFT))
          }
        }
      case '=' =>
        if (nextChar() != '=') {
          text.append("<=")
        } else {
          if (nextChar() == '>') {
            nextChar()
            addToResult(symbol("<==>", DOUBLE_ARROW_BOTH))
          } else {
            addToResult(symbol("<==", DOUBLE_ARROW_LEFT))
          }
        }
      case _ =>
        text.append('<')
    }
  }
}
