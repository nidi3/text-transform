package org.mimacom.text.transform

trait Parser {
  def parse(input: String): Segment
}