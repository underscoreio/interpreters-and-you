package church

trait ExprDsl {
  def lit[A](n: A): A

  def lt(a: Int, b: Int): Boolean

  def and(a: Boolean, b: Boolean): Boolean
}

object Program {
  def run(expr: ExprDsl): Boolean = {
    import expr._

    and(lt(lit(1), lit(2)), lt(lit(3), lit(4)))
  }
}

object Interpreter extends ExprDsl {
  def lit[A](n: A): A =
    n

  def lt(a: Int, b: Int): Boolean =
    a < b

  def and(a: Boolean, b: Boolean): Boolean =
    a && b
}

// The DSL is very precisely defined in this example.
// We can't really write alternative interpreters
// to compute other results or abstract over effects.

object Main extends App {
  println("Result: " + Program.run(Interpreter))
}
