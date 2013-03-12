package org.mimacom.text.transform.format

import org.mimacom.text.transform.Segment

trait ImageLoader {
  def loadImage(source: Segment, name: String): Option[String]
}
