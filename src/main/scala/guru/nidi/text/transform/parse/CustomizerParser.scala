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

import java.util.regex.Pattern

/**
 * Parse customizing attributes of an element, like the span in "|&lt;span=2>cell|".
 */
object CustomizerParser {
  def apply(pattern: Pattern, input: String, block: (String, String) => Unit) = {
    val matcher = pattern.matcher(input)
    var rest = input

    def find(): Boolean = {
      matcher.reset(rest)
      val found = matcher.find()
      if (found) {
        rest = rest.substring(0, matcher.start()) + rest.substring(matcher.end())
      }
      found
    }

    def name = matcher.group(1)

    def value = if (matcher.groupCount() == 3) matcher.group(3) else null

    while (find()) {
      block(name, value)
    }
    rest
  }
}
