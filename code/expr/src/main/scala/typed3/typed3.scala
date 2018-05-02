package typed3

sealed abstract class Expr[A] extends Product with Serializable
final case class Pure[A](value: A) extends Expr[A]
final case class FlatMap[A, B](a: Expr[A], fn: A => Expr[B]) extends Expr[B]

object Syntax {
  def lit[A](a: A): Expr[A] =
    Pure(a)

  implicit class IntExprOps(val a: Expr[Int]) extends AnyVal {
    def < (b: Expr[Int]): Expr[Boolean] =
      FlatMap(a, (a: Int) => FlatMap(b, (b: Int) => Pure(a < b)))
  }

  implicit class BoolExprOps(val a: Expr[Boolean]) extends AnyVal {
    def && (b: Expr[Boolean]): Expr[Boolean] =
      FlatMap(a, (a: Boolean) => FlatMap(b, (b: Boolean) => Pure(a && b)))
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
      case Pure(v)        => v
      case FlatMap(a, fn) => eval(fn(eval(a)))
    }
}

// object PrettyPrinter {
//   def print[A](program: Expr[A]): String =
//     program match {
//       case Lit(v)    => v.toString
//       case Lt(a, b)  => s"${print(a)} < ${print(b)}"
//       case And(a, b) => s"${print(a)} && ${print(b)}"
//     }
// }

// object Simplifier {
//   def simplify[A](program: Expr[A]): Expr[A] =
//     program match {
//       case Lit(n) =>
//         Lit(n)

//       case Lt(Lit(a), Lit(b)) =>
//         Lit(a < b)

//       case Lt(a, b) =>
//         Lt(simplify(a), simplify(b))

//       case And(a, b) =>
//         And(simplify(a), simplify(b))
//     }
// }

object Main extends App {
  import Program._

  println("Program: " + program)
  println("Result: " + Interpreter.eval(program))
}
