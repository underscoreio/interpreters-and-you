package chruch

trait ExprDsl {
  def lit[A](n: A): A
  def lt(a: Int, b: Int): Boolean
  def and(a: Boolean, b: Boolean): Boolean
}

object Program {
  def run(expr: ExprDsl): Boolean = {
    import expr._
    // 1 < 2 && 3 < 4
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

object Main extends App {
  println("Result: " + Program.run(Interpreter))
}
