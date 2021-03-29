trait CharacterSequence { val length: Int }

class Formatting[Word <: CharacterSequence] {

  sealed trait Token {
    val inlineLength: Int = this match {
      case One(word) => word.length
      case Pair((first, second), separator, open, close) =>
        open.length + first.length + separator.length + second.inlineLength + close.length
      case Many(terms, separator, open, close, trailingSeparator, isMulti) =>
        open.length + terms.map(term => term.inlineLength + separator.length).sum + close.length
    }
  }
  case class One(word: Word) extends Token
  case class Pair(terms: (Word, Token), separator: Word, open: Word, close: Word) extends Token
  case class Many(
      terms: Seq[Token],
      separator: Word,
      open: Word,
      close: Word,
      trailingSeparator: Boolean = true,
      isMulti: Boolean = false
  ) extends Token

  case class Writer(lines: Seq[Seq[Word]], currentColumn: Int) {
    def write(word: Word): Writer =
      Writer(
        currentColumn = this.currentColumn + word.length,
        lines = lines.slice(0, lines.length - 1).appended(lines.last.appended(word))
      )
    def endLine(): Writer =
      Writer(currentColumn = 0, lines = lines.appended(Seq()))
  }

  case class Format(maxColumns: Int, indentation: Word) {

    implicit class OnWriter(writer: Writer) {
      def indent(level: Int): Writer = Seq.fill(level)(indentation).foldLeft(writer)((w, i) => w.write(i))
      def formatInline(term: Token): Writer =
        term match {
          case One(word) => writer.write(word)
          case Pair((first, second), separator, open, close) =>
            writer.write(open).write(first).write(separator).formatInline(second).write(close)
          case Many(terms, separator, open, close, trailingSeparator, isMulti) =>
            terms.zipWithIndex
              .foldLeft(writer.write(open))((w, i) => {
                val (term, index) = i
                val withoutSeparator = w.formatInline(term)
                if (index == terms.length - 1 && !trailingSeparator) withoutSeparator
                else withoutSeparator.write(separator)
              })
              .write(close)
        }
      def formatMultiline(term: Token, level: Int): Writer =
        term match {
          case One(word) => writer.write(word)
          case Pair((first, second), separator, open, close) =>
            writer.write(open).write(first).write(separator).formatMixed(second, level).write(close)
          case Many(terms, separator, open, close, trailingSeparator, isMulti) =>
            terms.zipWithIndex
              .foldLeft(writer.write(open))((w, i) => {
                val (term, index) = i
                val withoutSeparator = w.endLine().indent(level).formatMixed(term, level)
                if (index == terms.length - 1 && !trailingSeparator) withoutSeparator
                else withoutSeparator.write(separator)
              })
              .endLine()
              .indent(level - 1)
              .write(close)
        }
      def formatMixed(term: Token, level: Int): Writer = {
        term match {
          case One(word) => writer.write(word)
          case Pair((first, second), separator, open, close) =>
            writer.write(open).write(first).write(separator).formatMixed(second, level).write(close)
          case Many(terms, separator, open, close, trailingSeparator, isMulti) =>
            if (isMulti && term.inlineLength > maxColumns - writer.currentColumn)
              writer.formatMultiline(term, level + 1)
            else
              terms.zipWithIndex
                .foldLeft(writer.write(open))((w, i) => {
                  val (term, index) = i
                  val remainingColumns = maxColumns - w.currentColumn
                  val withoutSeparator =
                    if (term.inlineLength <= remainingColumns) w.formatInline(term)
                    else w.formatMultiline(term, level + 1)
                  if (index == terms.length - 1 && !trailingSeparator) withoutSeparator
                  else withoutSeparator.write(separator)
                })
                .write(close)
        }
      }
    }
    def inline(term: Token): Writer = Writer.empty.formatInline(term = term)
    def mixed(term: Token): Writer = Writer.empty.formatMixed(term = term, level = 0)
    def multiline(term: Token): Writer = Writer.empty.formatMultiline(term = term, level = 0)
  }

  object Writer {
    def empty: Writer = Writer(currentColumn = 0, lines = Seq(Seq()))
  }
}
