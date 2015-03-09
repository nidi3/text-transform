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
package guru.nidi.text.transform.parse.wiki


import guru.nidi.text.transform.AttributeValue._
import guru.nidi.text.transform.Segment._
import guru.nidi.text.transform.TransformContext
import guru.nidi.text.transform.parse.{AbstractCharReadingParser, TextAccumulator}


/**
 *
 */
abstract class AbstractWikiParser(context: TransformContext) extends AbstractCharReadingParser(context) {
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
