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
package guru.nidi.text.transform

/**
 *
 */
class Name private(name: String) extends Const(name) {
  def apply(values: PseudoSegment*) = Segment(this, values: _*)
}

object Name {
  val ROOT = new Name("root")
  val PLAIN = new Name("plain")
  val BOLD = new Name("bold")
  val ITALICS = new Name("italics")
  val UNDERLINED = new Name("underlined")
  val HEADING = new Name("heading")
  val LINK = new Name("link")
  val LIST = new Name("list")
  val ITEM = new Name("item")
  val TABLE = new Name("table")
  val TABLE_CELL = new Name("tableCell")
  val LINE = new Name("line")
  val IMAGE = new Name("image")
  val SYMBOL = new Name("symbol")
  val NEWLINE = new Name("newline")
  val DEFINITION = new Name("definition")
}
