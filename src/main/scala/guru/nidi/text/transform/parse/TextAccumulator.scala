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

/**
 *
 */
class TextAccumulator() {
  private val text = new StringBuilder

  def append(s: String) {
    text.append(s)
  }

  def append(c: Char) {
    text.append(c)
  }

  def trim() {
    while (text.last <= ' ') text.deleteCharAt(text.length - 1)
  }

  def reset() {
    text.clear()
  }

  def endsWith(ends: String*) = ends.find(end => text.endsWith(end))

  def isEmptyOrNewline = text.length == 0 || text.charAt(text.length - 1) == '\n'

  def removeLast(n: Int) {
    text.setLength(text.length - n)
  }

  override def toString = text.toString()
}
