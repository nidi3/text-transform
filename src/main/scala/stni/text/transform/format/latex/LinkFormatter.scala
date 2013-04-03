package stni.text.transform.format.latex

import stni.text.transform.{TransformContext, Segment}
import stni.text.transform.Attribute._
import stni.text.transform.AttributeValue._

/**
 *
 */
object LinkFormatter {
  def format(context: TransformContext, segment: Segment) = {
    val target = segment(TARGET).get.asInstanceOf[String]

    def link() = {
      val ch = LatexFormatter.formatChildren(context, segment)
      s"\\href{$target}{$ch}"
    }

    def image() = {
      val msg = context.message("image")
      s"$msg \\ref{image:$target}"
    }

    def document() = {
      segment(SUB) match {
        case Some(seg: Segment) => LatexFormatter.formatChildren(context, seg)
        case _ => ""
      }
    }

    segment(TYPE) match {
      case Some(URL) => link()
      case Some(IMAGE_REF) => image()
      case Some(DOCUMENT_REF) => document()
      case _ => s"\\ref{$target}"
    }
  }
}
