package tagless

abstract class ExprAlg[F[_]] {
  def lit[A](n: A): F[A]
  def lt(a: F[Int], b: F[Int]): F[Boolean]
  def and(a: F[Boolean], b: F[Boolean]): F[Boolean]
}

object Program {
  def run[F[_]](expr: ExprAlg[F]): F[Boolean] = {
    import expr._
    // 1 < 2 && 3 < 4
    and(lt(lit(1), lit(2)), lt(lit(3), lit(4)))
  }
}

import cats.Id

object Interpreter extends ExprAlg[Id] {
  def lit[A](n: A): Id[A] =
    n

  def lt(a: Id[Int], b: Id[Int]): Id[Boolean] =
    a < b

  def and(a: Id[Boolean], b: Id[Boolean]): Id[Boolean] =
    a && b
}

case class Printed[A](text: String)

object PrettyPrinter extends ExprAlg[Printed] {
  def lit[A](n: A): Printed[A] =
    Printed(n.toString)

  def lt(a: Printed[Int], b: Printed[Int]): Printed[Boolean] =
    Printed(s"${a.text} < ${b.text}")

  def and(a: Printed[Boolean], b: Printed[Boolean]): Printed[Boolean] =
    Printed(s"${a.text} && ${b.text}")
}

object Reifier extends ExprAlg[typed.Expr] {
  def lit[A](n: A): typed.Expr[A] =
    typed.Lit(n)

  def lt(a: typed.Expr[Int], b: typed.Expr[Int]): typed.Expr[Boolean] =
    typed.Lt(a, b)

  def and(a: typed.Expr[Boolean], b: typed.Expr[Boolean]): typed.Expr[Boolean] =
    typed.And(a, b)
}

object Main extends App {
  println("Result: " + Program.run(Interpreter))
  println("Printed: " + Program.run(PrettyPrinter))
  println("Reified: " + Program.run(Reifier))
}
