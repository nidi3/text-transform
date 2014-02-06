package stni.text.transform.parse

import stni.text.transform.{TransformContext, Segment, Parser}

/**
 *
 */
abstract class AbstractParser(val context: TransformContext) extends Parser {

  private var _input: String = ""

  def parse(input: String): Segment = {
    _input = input
    parseImpl()
  }

  def parseImpl(): Segment

  def parseSub(sub: String, subContext: TransformContext = context): Segment = newInstance(subContext).parse(sub)

  protected def newInstance(subContext: TransformContext = context): AbstractParser = {
    try {
      getClass.getConstructor(classOf[TransformContext]).newInstance(subContext)
    } catch {
      case e: Exception => throw new RuntimeException("Could not create a new instance of " + getClass, e)
    }
  }

  def input = _input
}
