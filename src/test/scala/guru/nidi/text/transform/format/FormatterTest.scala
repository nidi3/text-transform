package guru.nidi.text.transform.format

import guru.nidi.text.transform.{Formatter, Segment}
import guru.nidi.text.transform.Name._
import org.scalatest.FlatSpec

/**
 *
 */
trait FormatterTest extends FlatSpec {
  def formatter: Formatter

  class FormatSource(input: String) {
    def formatOf(segment: Segment) {
      if (segment.name == ROOT) {
        assert(formatter.format(segment) === input)
      } else {
        assert(formatter.format(ROOT(segment)) === input)
      }
    }
  }

  implicit def string2formatSource(s: String) = new FormatSource(s)

}
