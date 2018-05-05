# Understanding interpreters

General-purpose programming languages provide generality,
which is both an advantage and a disadvantage.
However, if we're trying to solve a specific problem
in a specific domain it sometimes helps to remove the generality
and work with a limited set of high-level primitives.
We call this a *Domain Specific Language (DSL)*,
which we embed inside a general purpose *host language*.
Working with DSLs involves splitting code into two halves:

1. Write a "program" using the DSL that
   represents the code we want to run.

2. Feed the program to an *interpreter*---a function that
   accepts a program as input and produces a useful result.

The program in step 1 simply represents
the *structure* of the computation we want to run.
We leave underlying details to the interpreter.
This approach gives us a number of advantages:

- Our DSL is simpler than a general purpose programming language,
  making it easier for us to build programs within the DSL's domain.

- The interpreter takes care of messy underlying complexities.
  We can re-use the same interpreter to run different programs.

- We can invent different *syntaxes* to simplify writing
  different types of programs.

- We can invent different interpreters
  to execute programs in different ways.
  For example to compute different results
  or handle different effects.

The general pattern looks like this:

```
Input syntax 1 --\                /--> Interpreter 1 --> Result 1
                  \      DSL     /
Input syntax 2 ------> Program ------> Interpreter 2 --> Result 2
                  /              \
Input syntax 3 --/                \--> Interpreter 3 --> Result 3
```

The interpreter pattern can also open up a number of advantages
in terms of inspectability of code.
By limiting the expressiveness of the DSL,
we increase our ability to reason about programs in our interpreter.
As a [wise man once said](link-runar-constraints),
"constraints liberate, liberty constrains".
If we plan correctly we can write *compilers*
that change the structure of programs
to make them more efficient or to handle additional complexities.
In some cases we can even serialize programs
to save them for later or run them outside of our host language.

## A calculated example

Let's consider a simple example:
writing a calculator that supports
integers, addition, and multiplication.
Any general-purpose language will have syntax and semantics for this.
Here's a Scala "program" to perform a simple calculation.
We represent the program as a function
so we can manipulate it as a value:

```tut:book:silent
val program = () =>
  (1 + 2) * (3 + 4)
```

We can run this program simply by calling it:

```tut:book
program()
```

We can even write an "interpreter" to run the program for us:

```tut:book:silent
def eval(prog: () => Int): Int =
  prog()
```

```tut:book
eval(program)
```

On a shallow level, the definitions of `program` and `eval`
follow the interpreter pattern we laid out above:
`program` represents the structure of a calculation,
and `eval` runs the calculation for us.

However, this isn't a particularly useful
implementation of the interpreter pattern.
Consider the following questions:

1. What information can we gather about a program from inside our interpreter?

2. How many alternative implementations of `eval` can we imagine?

You should quickly realise that the only really useful thing
we can do with a Scala function is call it.
We can't inspect it, we can't serialize it,
and we can't change the result type.

Furthermore, it's impossible to predict what any given program will do.
Programs have all the power of Scala at their disposal.
They can spawn threads, do arbitrary I/O,
and consume any number of system resources.
There's very little our interpreter can do
to detect this, reason about it, or prevent it.

Now suppose we introduce a DSL for representing calculations:

```tut:book:silent
sealed trait Expr
final case class Lit(value: Int) extends Expr
final case class Add(a: Expr, b: Expr) extends Expr
final case class Mul(a: Expr, b: Expr) extends Expr
```

Rather than implementing programs as Scala functions,
we implement them as values of type `Expr`:

```tut:book:silent
val program: Expr =
  Mul(Add(Lit(1), Lit(2)), Add(Lit(3), Lit(4)))
```

Here's an interpreter to evaluate programs in this new DSL:

```tut:book:silent
def eval(expr: Expr): Int =
  expr match {
    case Lit(value) => value
    case Add(a, b)  => eval(a) + eval(b)
    case Mul(a, b)  => eval(a) * eval(b)
  }
```

```tut:book
eval(program)
```

By replacing a general language like Scala with a simple data type like `Expr`,
we restrict massively the types of computations we can represent in our programs.
However, in doing so we open up lots of options for interpretation.
We can make an asynchronous version of `eval`:

```tut:book:silent
import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
```

```tut:book:silent
def evalAsync(expr: Expr): Future[Int] =
  expr match {
    case Lit(value) => Future.successful(value)
    case Add(a, b)  => evalAsync(a).flatMap(a => evalAsync(b).map(b => a + b))
    case Mul(a, b)  => evalAsync(a).flatMap(a => evalAsync(b).map(b => a * b))
  }
```

```tut:book
val result = evalAsync(program)

Await.result(result, 1.second)
```

We can produce alternative results from the same program:

```tut:book:silent
def prettyPrint(expr: Expr): String =
  expr match {
    case Lit(value) => value.toString
    case Add(a, b)  => s"${prettyPrint(a)} + ${prettyPrint(b)}"
    case Mul(a, b)  => s"${prettyPrint(a)} * ${prettyPrint(b)}"
  }
```

```tut:book
prettyPrint(program)
```

We can even rewrite programs to change the execution structure:

```tut:book:silent
def simplify(expr: Expr): Expr =
  expr match {
    case Mul(a, Add(b, c)) => simplify(Add(Mul(a, b), Mul(a, c)))
    case Mul(Add(a, b), c) => simplify(Add(Mul(a, c), Mul(b, c)))
    case Mul(a, b)         => Mul(simplify(a), simplify(b))
    case Add(a, b)         => Add(simplify(a), simplify(b))
    case Lit(v)            => Lit(v)
  }
```

```tut:book
prettyPrint(simplify(program))
```

This neatly demonstrates the value of the interpreter pattern.
By restricting the expressiveness of our DSL,
we expand the options for what we can do in our interpreter.
We can provide a set of interpreters to handle all sorts of complex situations,
and re-use all of that code again and again to execute new programs.

## Standalone and embedded DSLs

TODO

## Reification versus Church encoding

TODO

# Reified models

TODO

## Untyped interpreters

TODO

## Typed interpreters

TODO

## Monadic interpreters

TODO

## The free monad

TODO

# Church encoded models

TODO

## Direct church encoding

TODO

## Tagless final encoding

TODO

# Other considerations

TODO

## Inspection and rewriting

TODO

## Serialization

TODO

# Conclusion

TODO
