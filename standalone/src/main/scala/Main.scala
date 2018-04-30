import fastparse.all._

sealed abstract class Expr extends Product with Serializable
final case class Add(a: Expr, b: Expr) extends Expr
final case class Multiply(a: Expr, b: Expr) extends Expr
final case class Literal(n: Int) extends Expr

object Parser {
  def parse(text: String): Expr =
    expr.parse(text).get.value

  val expr: Parser[Expr] =
    P(add ~ End)

  val add: Parser[Expr] =
    P(mul ~ (ws ~ "+" ~ ws ~ mul).rep)
      .map { case (head, tail) => tail.foldLeft(head)(Add) }

  val mul: Parser[Expr] =
    P(term ~ (ws ~ "*" ~ ws ~ term).rep)
      .map { case (head, tail) => tail.foldLeft(head)(Multiply) }

  val term: Parser[Expr] =
    P(paren | lit)

  val paren: Parser[Expr] =
    P("(" ~ ws ~ add ~ ws ~ ")")

  val lit: Parser[Expr] =
    P(CharIn("+-").? ~ CharIn('0' to '9').rep(1)).!
      .map(_.toInt).map(Literal)

  val ws: Parser[Unit] =
    P(CharIn(" \t").rep)
}

object Program {
  import Parser.parse

  val program =
    parse("""1 * 2 + 3 * 4""")
}

object Interpreter {
  def eval(program: Expr): Int =
    program match {
      case Literal(n)     => n
      case Add(a, b)      => eval(a) + eval(b)
      case Multiply(a, b) => eval(a) * eval(b)
    }
}

object Main extends App {
  import Program.program
  import Interpreter.eval

  println("Program: " + program)
  println("Result: " + eval(program))
}
