package typed

sealed abstract class Expr[A] extends Product with Serializable
final case class Lit[A](value: A) extends Expr[A]
final case class Lt(a: Expr[Int], b: Expr[Int]) extends Expr[Boolean]
final case class And(a: Expr[Boolean], b: Expr[Boolean]) extends Expr[Boolean]

object Program {
  val program: Expr[Boolean] =
    And(Lt(Lit(1), Lit(2)), Lt(Lit(3), Lit(4)))
}

object Interpreter {
  def eval[A](program: Expr[A]): A =
    program match {
      case Lit(v) =>
        v

      case Lt(a, b) =>
        eval(a) < eval(b)

      case And(a, b) =>
        eval(a) && eval(b)
    }
}

object AsyncInterpreter {
  import scala.concurrent.{Future, ExecutionContext}

  def eval[A](program: Expr[A])(implicit ec: ExecutionContext): Future[A] =
    program match {
      case Lit(v) =>
        Future.successful(v)

      case Lt(a, b) =>
        val x = eval(a)
        val y = eval(b)
        x.flatMap(a => y.map(b => a < b))

      case And(a, b) =>
        val x = eval(a)
        val y = eval(b)
        x.flatMap(a => y.map(b => a && b))
    }
}

object PrettyPrinter {
  def print[A](program: Expr[A]): String =
    program match {
      case Lit(v) =>
        v.toString

      case Lt(a, b) =>
        s"${print(a)} < ${print(b)}"

      case And(a, b) =>
        s"${print(a)} && ${print(b)}"
    }
}

object Simplifier {
  def simplify[A](program: Expr[A]): Expr[A] =
    program match {
      case Lit(n) =>
        Lit(n)

      case Lt(Lit(a), Lit(b)) =>
        Lit(a < b)

      case Lt(a, b) =>
        Lt(simplify(a), simplify(b))

      case And(a, b) =>
        And(simplify(a), simplify(b))
    }
}

object Main extends App {
  import Program._

  import scala.concurrent.ExecutionContext.Implicits.global

  println("Program: " + program)
  println("Result: " + Interpreter.eval(program))
  println("Async result: " + AsyncInterpreter.eval(program))
  println("Printed: " + PrettyPrinter.print(program))
  println("Simplified: " + Simplifier.simplify(program))
}
