package org.mimacom.text.transform.format.latex

import org.mimacom.text.transform.Segment;
import org.mimacom.text.transform.format.ImageLoader;

import java.text.MessageFormat;
import java.util.Locale;
import java.util.ResourceBundle;

/**
 *
 */
class Context(val headingLevel: Int, locale: Locale, imageLoader: ImageLoader) {
  val messages = ResourceBundle.getBundle(getClass.getPackage.getName + ".messages", locale)

  def message(key: String, parameters: AnyRef*) = MessageFormat.format(messages.getString(key), parameters.toArray:_*)

  def loadImage(source: Segment, name: String):Option[String] = imageLoader.loadImage(source, name)
}
