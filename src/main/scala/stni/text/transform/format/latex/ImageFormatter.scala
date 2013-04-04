package stni.text.transform.format.latex

import stni.text.transform.{TransformContext, Segment}
import stni.text.transform.Attribute._
import stni.text.transform.format.latex.LatexFormatter.env
import stni.text.transform.format.latex.LatexFormatter.formatChildren

/**
 * Formats an image element.
 */
object ImageFormatter {
  def format(context: TransformContext, segment: Segment): String = {
    val image = segment(TARGET).get.asInstanceOf[String]
    context.loadResource(segment, image) match {
      case None => context.message("imageNotFound", image)
      case Some(imageName) =>
        val name = formatChildren(context, segment)
        val option = createOptionString(segment)
        segment(FLOAT) match {
          case Some(true) => transformFloatEnvironment(name, imageName, option)
          case _ => transformNonfloatEnvironment(name, imageName, option)
        }
    }
  }

  private def createOptionString(segment: Segment): String = {
    def handlePercent(value: String, percentBase: String): String = {
      def calcPercent = {
        try {
          Integer.parseInt(value.substring(0, value.length() - 1))
        } catch {
          case _: NumberFormatException => 100
        }
      }
      if (value.endsWith("%")) (calcPercent / 100.0) + percentBase else value
    }

    def handleWidth(value: String): String = {
      ",width=" + handlePercent(value, "\\textwidth")
    }

    val height = segment(HEIGHT) match {
      case Some(h: String) => ",height=" + handlePercent(h, "\\textheight")
      case _ => ""
    }
    val width = segment(WIDTH) match {
      case Some(w: String) => handleWidth(w)
      case _ => if (height == "") handleWidth("100%") else ""
    }
    val angle = segment(ANGLE) match {
      case Some(a: String) => ",angle=" + a
      case _ => ""
    }

    (height + width + angle).substring(1)
  }

  private def transformFloatEnvironment(name: String, imageName: String, options: String) = {
    val fig = transformFigure(imageName, options)
    env("figure","[hpt]") {
      s"\\centering\n$fig\\caption{$name} \\label{image:$name}\n"
    }
  }

  private def transformNonfloatEnvironment(name: String, imageName: String, options: String) = {
    val fig = transformFigure(imageName, options)
    env("center") {
      env("minipage","{\\linewidth}") {
        s"$fig\\captionof{figure}{$name} \\label{image:$name}\n"
      }
    }
  }

  private def transformFigure(imageName: String, options: String) = s"\\includegraphics[$options]{$imageName}\n"

}
