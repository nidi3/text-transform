package guru.nidi.text.transform.format.latex

import guru.nidi.text.transform.{TransformContext, Segment}
import guru.nidi.text.transform.Attribute._
import guru.nidi.text.transform.format.latex.LatexFormatter.env

/**
 * Formats an image element.
 */
object ImageFormatter {
  def format(context: TransformContext, segment: Segment): String = {
    val image = segment(TARGET).get
    context.loadResource(segment, image) match {
      case None => context.message("imageNotFound", new LatexFormatter(context), image)
      case Some(imageName) =>
        val caption = LatexFormatter.formatCaption(context, segment)
        val option = createOptionString(segment)
        segment(FLOAT) match {
          case Some(true) => transformFloatEnvironment(caption, transformFigure(imageName, option), LatexFormatter.formatLabel(segment(ID)))
          case _ => transformNonfloatEnvironment(caption, transformFigure(imageName, option), LatexFormatter.formatLabel(segment(ID)))
        }
    }
  }

  private def createOptionString(segment: Segment): String = {
    def handlePercent(value: String, percentBase: String): String = {
      def calcPercent = {
        try {
          java.lang.Double.parseDouble(value.substring(0, value.length() - 1))
        } catch {
          case _: NumberFormatException => 100.0
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

  private def transformFloatEnvironment(caption: String, figure: String, formattedLabel: String) = {
    env("figure", "[hpt]") {
      s"\\centering\n$figure\\caption{$caption} $formattedLabel\n"
    }
  }

  private def transformNonfloatEnvironment(caption: String, figure: String, formattedLabel: String) = {
    """~\\\\""" +
      env("minipage", "{\\linewidth}") {
        env("center") {
          s"$figure\\captionof{figure}{$caption} $formattedLabel\n"
        }
      } +
      """\par\bigskip""" + "\n"
  }

  private def transformFigure(source: String, options: String) = s"\\includegraphics[$options]{$source}\n"

}
