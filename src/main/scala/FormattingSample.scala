import react.Helpers._
import react.ReactNode

object FormattingSample {
  def viewFormatted(maxColumns: Int): ReactNode = {
    val formatted = formatting.Format(maxColumns, "  ").mixed(formattingSample)
    div()(
      for ((line, lineIndex) <- formatted.lines.zipWithIndex)
        yield div(key = lineIndex.toString)(
          for ((word, wordIndex) <- line.zipWithIndex) yield span(key = wordIndex.toString)(word.string)
        )
    )
  }
  case class StyledString(string: String) extends CharacterSequence {
    val length: Int = string.length
  }
  implicit def stringToStyledString(string: String): StyledString = StyledString(string)
  val formatting: Formatting[StyledString] = new Formatting[StyledString]
  val formattingSample: formatting.Token = {
    import formatting.{Many, One, Token}
    implicit def stringToWord(string: String): Token = One(StyledString(string))
    def app(terms: Token*): Token = Many(terms, ", ", "(", ")", trailingSeparator = false)
    app(
      app("a", "b", "c"),
      app("aaaaaaaaaa", "bbbbbbbbbb", "cccccccccc"),
      app("a", "b", "c"),
      app("aaaaaaaaaa", app(app("a", "b", "c"), app("aaaaaaaaaa", "bbbbbbbbbb", "cccccccccc")), "cccccccccc")
    )
  }
}
