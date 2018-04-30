sealed abstract class Expr extends Product with Serializable
final case class Add(a: Expr, b: Expr) extends Expr
final case class Multiply(a: Expr, b: Expr) extends Expr
final case class Literal(n: Int) extends Expr

object Syntax {
  implicit class ExprOps(val a: Expr) extends AnyVal {
    def + (b: Expr): Expr =
      Add(a, b)

    def * (b: Expr): Expr =
      Multiply(a, b)
  }

  def lit(n: Int): Expr =
    Literal(n)
}

object Program {
  import Syntax._

  val program =
    lit(1) * lit(2) + lit(3) * lit(4)
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
