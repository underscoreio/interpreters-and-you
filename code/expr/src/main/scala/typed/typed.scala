package typed

sealed abstract class Expr[A] extends Product with Serializable
final case class Lit[A](value: A) extends Expr[A]
final case class Lt(a: Expr[Int], b: Expr[Int]) extends Expr[Boolean]
final case class And(a: Expr[Boolean], b: Expr[Boolean]) extends Expr[Boolean]

object Syntax {
  def lit[A](a: A): Expr[A] =
    Lit(a)

  implicit class IntExprOps(val a: Expr[Int]) extends AnyVal {
    def < (b: Expr[Int]): Expr[Boolean] =
      Lt(a, b)
  }

  implicit class BoolExprOps(val a: Expr[Boolean]) extends AnyVal {
    def && (b: Expr[Boolean]): Expr[Boolean] =
      And(a, b)
  }
}

object Program {
  import Syntax._

  val program: Expr[Boolean] =
    lit(1) < lit(2) && lit(3) < lit(4)
}

object Interpreter {
  def eval[A](program: Expr[A]): A =
    program match {
      case Lit(v)    => v
      case Lt(a, b)  => eval(a) < eval(b)
      case And(a, b) => eval(a) && eval(b)
    }
}

object PrettyPrinter {
  def print[A](program: Expr[A]): String =
    program match {
      case Lit(v)    => v.toString
      case Lt(a, b)  => s"${print(a)} < ${print(b)}"
      case And(a, b) => s"${print(a)} && ${print(b)}"
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

  println("Program: " + program)
  println("Result: " + Interpreter.eval(program))
  println("Printed: " + PrettyPrinter.print(program))
  println("Simplified: " + Simplifier.simplify(program))
}
