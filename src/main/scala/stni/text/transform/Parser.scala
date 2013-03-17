package stni.text.transform

trait Parser {
  def parse(input: String): Segment
}