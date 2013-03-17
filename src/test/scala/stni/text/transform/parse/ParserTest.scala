package stni.text.transform.parse

import stni.text.transform.{Parser, Segment}
import stni.text.transform.Name._
import org.scalatest.FlatSpec

/**
 *
 */
trait ParserTest extends FlatSpec {
  def parser: Parser

  class ParseSource(input: String) {
    def parseTo(segment: Segment) {
      val parsed = parser.parse(input)
      if (segment.name == ROOT) {
        assert(parsed === segment)
      } else {
        assert(parsed.children.size == 1)
        assert(parsed.children(0) === segment)
      }
    }
  }

  implicit def string2parseSource(s: String) = new ParseSource(s)

}
