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
 *
 */
object LinkFormatter {
  def format(context: TransformContext, segment: Segment) = {
    val target = segment(TARGET).get

    def link = {
      val ch = LatexFormatter.formatCaption(context, segment)
      val (text, rawUrl) = context.processLink(segment, ch, target)
      val url = escape(rawUrl)
      s"\\href{$url}{$text}"
    }

    def escape(url: String) = url.replace("%", "\\%")

    def document = {
      segment(SUB) match {
        case Some(seg: Segment) => LatexFormatter.formatChildren(context, seg)
        case _ => ""
      }
    }

    segment(TYPE) match {
      case Some(URL) => link
      case Some(DOCUMENT_REF) => document
      case Some(DOCUMENT_INCLUDE) => LatexFormatter.formatChildren(context, segment)
      case Some(FILE_REF) => LatexFormatter.formatChildren(context, segment) + s"\\label{$target}"
      case _ => s"\\autoref{$target}"
    }
  }
}
