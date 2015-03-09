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
object TagCustomizerParser {
  private val PATTERN = Pattern.compile("<([A-Za-z-]+)(=([^>]+))?>")

  def apply(input: String, block: (String, String) => Unit) = CustomizerParser.apply(PATTERN, input, block)
}
