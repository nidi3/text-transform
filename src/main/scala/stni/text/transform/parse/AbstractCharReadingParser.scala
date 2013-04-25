package stni.text.transform.parse

import collection.mutable.ListBuffer
import stni.text.transform.{TransformContext, Segment}
import stni.text.transform.Name._
import AbstractCharReadingParser.EOI

/**
 *
 */
object AbstractCharReadingParser {
  val EOI: Char = -1.asInstanceOf[Char]
}

abstract class AbstractCharReadingParser(context: TransformContext) extends AbstractParser(context) {

  private var c: Char = 0
  private var pos: Int = 0
  private var savedC: Char = 0
  private var savedPos: Int = 0

  val result = ListBuffer[Segment]()

  override def parseImpl(): Segment = {
    pos = 0
    nextChar()
    reset()
    val root = ROOT()
    doParse()
    root(result: _*)
  }

  def doParse()

  protected def reset() {
    result.clear()
  }

  def addToResult(segment: Segment) {
    result += segment
  }

  protected def addToResult(pos: Int, segment: Segment) {
    result.insert(pos, segment)
  }

  def isCurrentCharOneOf(s: String): Boolean = s.indexOf(c) >= 0

  def nextChar(): Char = {
    c = fetchNextChar()
    c
  }

  private def fetchNextChar(): Char = {
    if (pos >= input.length) {
      EOI
    } else {
      if (c == '\n') {
        var d: Char = 0
        do {
          d = input(pos)
          pos += 1
        } while ((d == ' ' || d == '\t') && pos < input.length)
        if (d == ' ' || d == '\t') EOI else d
      } else {
        pos += 1
        input(pos - 1)
      }
    }
  }

  def currentChar = c

  def pushBack(n: Int) {
    //TODO take care of "space after newline" logic, see #nextChar
    pos -= n
    if (pos < input.length) {
      c = input(pos-1)
    }
  }

  def savePos() {
    savedPos = pos
    savedC = c
  }

  def restorePos() = {
    c = savedC
    pos = savedPos
    nextChar()
    savedC
  }

  //
  //  public String lookAhead(int n) {
  //    savePos();
  //    StringBuffer s = new StringBuffer();
  //    for (int i = 0; i < n; i++) {
  //      s.append(nextChar());
  //    }
  //    restorePos();
  //    return s.toString();
  //  }
  //
  //  public boolean isCurrentCharOneOf(String s) {
  //    return s.indexOf(c) >= 0;
  //  }
  //
  //
  //
  /**
   * Read the input until one of the given Strings occurs.
   *
   * @param ends the Strings to be searched
   * @return the input until but not including the first found String
   */
  def readUntil(ends: String*): String = {
    val res = new StringBuilder
    val endpos = new Array[Int](ends.length)
    var foundIndex = -1
    do {
      res.append(c)
      for (i <- 0 until ends.length) {
        if (c == ends(i).charAt(endpos(i))) {
          endpos(i) += 1
          if (endpos(i) == ends(i).length) {
            foundIndex = i
          }
        } else {
          endpos(i) = 0
        }
      }
      nextChar()
    } while (c != EOI && foundIndex == -1)
    if (foundIndex != -1) {
      res.delete(res.length - ends(foundIndex).length, res.length)
    }
    res.toString()
  }

  def readUntilChar(chars: String): String = {
    val res = new StringBuilder
    do {
      res.append(c)
      nextChar()
    } while (chars.indexOf(c) < 0 && c != EOI)
    res.toString()
  }

  def getCount(sym: Char) = {
    var level = 1
    while (currentChar == sym) {
      nextChar()
      level += 1
    }
    level
  }

  def skipWhitspaces() {
    while (currentChar <= ' ') {
      nextChar()
    }
  }
}
