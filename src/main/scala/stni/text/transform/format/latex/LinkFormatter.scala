package stni.text.transform.format.latex

import stni.text.transform.{TransformContext, Segment}
import stni.text.transform.Attribute._
import stni.text.transform.AttributeValue._

/**
 *
 */
object LinkFormatter {
  def format(context: TransformContext, segment: Segment) = {
    val target = segment(TARGET).get

    def link = {
      val ch = LatexFormatter.formatCaption(context, segment)
      val (text, url) = context.processLink(segment, ch, target)
      s"\\href{$url}{$text}"
    }

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
