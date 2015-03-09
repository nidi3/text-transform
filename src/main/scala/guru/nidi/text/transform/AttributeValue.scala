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
class AttributeValue private(name: String) extends Const(name)

object AttributeValue {
  val ORDERED = new AttributeValue("ordered")
  val UNORDERED = new AttributeValue("unordered")
  val ARROW_LEFT = new AttributeValue("larr")
  val ARROW_RIGHT = new AttributeValue("rarr")
  val ARROW_BOTH = new AttributeValue("barr")
  val DOUBLE_ARROW_LEFT = new AttributeValue("dlarr")
  val DOUBLE_ARROW_RIGHT = new AttributeValue("drarr")
  val DOUBLE_ARROW_BOTH = new AttributeValue("dbarr")
  val LEFT = new AttributeValue("left")
  val RIGHT = new AttributeValue("right")
  val FILE_REF = new AttributeValue("file-ref")
  val DOCUMENT_REF = new AttributeValue("document-ref")
  val DOCUMENT_INCLUDE = new AttributeValue("document-include")
  val IMAGE_REF = new AttributeValue("image-ref")
  val REF = new AttributeValue("ref")
  val URL = new AttributeValue("url")

  def leftOrRight(s: String) = s match {
    case "left" => LEFT
    case "right" => RIGHT
    case _ => UNORDERED
  }
}