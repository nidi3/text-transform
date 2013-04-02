package stni.text.transform.parse

import stni.text.transform.{Context, Segment, Parser}

/**
 *
 */
abstract class AbstractParser(val context: Context) extends Parser {

  private var _input: String = ""

  def parse(input: String): Segment = {
    _input = input
    parseImpl
  }

  def parseImpl(): Segment

  def parseSub(sub: String, subContext: Context = context): Segment = newInstance(subContext).parse(sub)

  protected def newInstance(subContext: Context = context): AbstractParser = {
    try {
      getClass.getConstructor(classOf[Context]).newInstance(subContext)
    } catch {
      case e: Exception => throw new RuntimeException("Could not create a new instance of " + getClass, e)
    }
  }

  def input = _input
}
