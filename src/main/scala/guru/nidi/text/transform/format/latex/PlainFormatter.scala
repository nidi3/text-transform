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
import guru.nidi.text.transform.{Segment, TransformContext}

/**
 * Formats plain text by handling escaping issues.
 */
object PlainFormatter {
  private val REPLACEMENT = Map(
    '\\' -> "\\textbackslash ",
    '#' -> "\\#",
    '$' -> "\\$",
    '%' -> "\\%",
    '&' -> "\\&",
    '~' -> "\\~{}",
    '_' -> "\\_",
    '^' -> "\\^{}",
    '{' -> "\\{",
    '}' -> "\\}",
    '[' -> "{[}",
    ']' -> "{]}",
    160 -> '~') //non-break space (&nbsp; in HTML)

  def format(context: TransformContext, segment: Segment) = escaped(segment(TEXT).get)

  private def escaped(s: String): String = {
    val res = new StringBuilder
    var inDoubleQuotes = false
    s.foreach(c => {
      REPLACEMENT.get(c) match {
        case Some(replace) =>
          res.append(replace)
        case _ =>
          c match {
            case '"' =>
              res.append(if (inDoubleQuotes) "\"'" else "\"`")
              inDoubleQuotes = !inDoubleQuotes
            case '„' =>
              inDoubleQuotes = true
              res.append('„')
            case _ =>
              res.append(c)
          }
      }
    })
    res.toString()
  }
}
