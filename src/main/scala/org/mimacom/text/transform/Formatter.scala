package org.mimacom.text.transform

trait Formatter {
  def format(segment: Segment): String
}