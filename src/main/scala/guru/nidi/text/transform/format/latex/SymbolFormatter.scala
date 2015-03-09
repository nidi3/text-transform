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
package guru.nidi.text.transform.format.latex

import guru.nidi.text.transform.Attribute._
import guru.nidi.text.transform.AttributeValue._
import guru.nidi.text.transform.{Segment, TransformContext}

/**
 * Formats an image element.
 */
object SymbolFormatter {

  val symbols = Map(
    ARROW_LEFT -> "$\\leftarrow$ ",
    ARROW_RIGHT -> "$\\rightarrow$ ",
    ARROW_BOTH -> "$\\leftrightarrow$ ",
    DOUBLE_ARROW_LEFT -> "$\\Leftarrow$ ",
    DOUBLE_ARROW_RIGHT -> "$\\Rightarrow$ ",
    DOUBLE_ARROW_BOTH -> "$\\Leftrightarrow$ "
  )

  def format(context: TransformContext, segment: Segment) =
    symbols.get(segment(TYPE).get) match {
      case None => segment(ORIGINAL).get
      case Some(s) => s
    }
}

