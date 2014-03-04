package guru.nidi.text.transform.parse

import guru.nidi.text.transform.{Parser, Segment}
import guru.nidi.text.transform.Name._
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
