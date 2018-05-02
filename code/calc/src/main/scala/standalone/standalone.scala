package standalone

import reified.{
  Expr,
  Lit,
  Add,
  Mul,
  Interpreter,
  AsyncInterpreter,
  GeneralInterpreter,
  PrettyPrinter,
  Simplifier
}

//noinspection ForwardReference
object Parser {
  import fastparse.all._

  def parse(text: String): Expr =
    expr.parse(text).get.value

  private val expr: Parser[Expr] =
    P(add ~ End)

  private val add: Parser[Expr] =
    P(mul ~ (ws ~ "+" ~ ws ~ mul).rep)
      .map { case (head, tail) => tail.foldLeft(head)(Add) }

  private val mul: Parser[Expr] =
    P(term ~ (ws ~ "*" ~ ws ~ term).rep)
      .map { case (head, tail) => tail.foldLeft(head)(Mul) }

  private val term: Parser[Expr] =
    P(paren | lit)

  private val paren: Parser[Expr] =
    P("(" ~ ws ~ add ~ ws ~ ")")

  private val lit: Parser[Expr] =
    P(CharIn("+-").? ~ CharIn('0' to '9').rep(1)).!
      .map(_.toInt).map(Lit)

  private val ws: Parser[Unit] =
    P(CharIn(" \t").rep)
}

object Program {
  import Parser.parse

  val program1: Expr =
    parse("""1 * 2 + 3 * 4""")

  val program2: Expr =
    parse("""(1 + 2) * (3 + 4)""")
}

object Main extends App {
  import Program._

  println("Program: " + program1)
  println("Result: " + Interpreter.eval(program1))
  println("Printed: " + PrettyPrinter.print(program1))

  import scala.concurrent.Await
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  println("Async result: " + Await.result(AsyncInterpreter.eval(program1), 1.second))

  import cats.instances.all._

  println("Optional result: " + GeneralInterpreter.eval[Option](program1))

  println("Simplified: " + Simplifier.simplify(program2))
}
