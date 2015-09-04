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
package guru.nidi.text.transform.format

import guru.nidi.text.transform.Name._
import guru.nidi.text.transform.{Formatter, Segment}
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

  implicit def string2formatSource(s: String): FormatSource = new FormatSource(s)

}
