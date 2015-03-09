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
package guru.nidi.text.transform.parse

import guru.nidi.text.transform.Name._
import guru.nidi.text.transform.{Parser, Segment}
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
