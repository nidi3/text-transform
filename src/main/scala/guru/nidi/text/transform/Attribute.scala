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
class Attribute[T] private(name: String) extends Const(name) {
  def apply(index: Int) = new Attribute[T](name + index)

  def ->(value: T) = new AttributePair(this, value)
}

object Attribute {
  val TEXT = Attribute[String]("text")
  val LEVEL = Attribute[Int]("level")
  val TARGET = Attribute[String]("target")
  val COLUMNS = Attribute[Int]("columns")
  val ROWS = Attribute[Int]("rows")
  val FLOAT = Attribute[Boolean]("float")
  val ORIGINAL = Attribute[String]("original")
  val TYPE = Attribute[AttributeValue]("type")
  val SPAN = Attribute[Int]("span")
  val WIDTH = Attribute[String]("width")
  val HEIGHT = Attribute[String]("height")
  val ANGLE = Attribute[String]("angle")
  val ALIGN = Attribute[AttributeValue]("align")
  val HEADER = Attribute[Boolean]("header")
  val CAPTION = Attribute[Segment]("caption")
  val SUB = Attribute[Segment]("sub")
  val ID = Attribute[String]("id")

  def apply[T](name: String) = new Attribute[T](name)
}

