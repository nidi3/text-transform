package stni.text.transform


import java.text.MessageFormat
import java.util.Locale
import java.util.ResourceBundle

/**
 *
 */
class TransformContext(val headingLevel: Int, locale: Locale, resourceLoader: ResourceLoader) {
  val messages = ResourceBundle.getBundle(classOf[TransformContext].getPackage.getName + ".messages", locale)

  def message(key: String, parameters: AnyRef*) = MessageFormat.format(messages.getString(key), parameters.toArray: _*)

  def loadResource(source: Segment, name: String): Option[String] = resourceLoader.loadResource(source, name)

  def subContext = new TransformContext(headingLevel + 1, locale, resourceLoader)

  def includeSub(parent: Segment, sub: Segment) = sub

  def processLink(source: Segment, text: String, url: String): Pair[String, String] = (text, url)
}


