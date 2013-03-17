package stni.text.transform

trait Formatter {
  def format(segment: Segment): String
}