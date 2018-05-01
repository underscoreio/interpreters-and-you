object Program {
  val program1 = () =>
    1 * 2 + 3 * 4
}

object Interpreter {
  def eval(program: () => Int): Int =
    program()
}

object Main extends App {
  import Program._

  println("Program: " + program1)

  import Interpreter._

  println("Result: " + eval(program1))
}
