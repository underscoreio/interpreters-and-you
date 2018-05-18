# Understanding interpreters

General-purpose programming languages provide generality,
which is both an advantage and a disadvantage.
While we can adapt them to any task,
we often have to lay groundwork to achieve those tasks
in the form of libraries and implementation details.
If we're trying to solve problems in a specific domain
it sometimes helps to remove the generality
and work with a language with a
limited set of high-level primitives.
We call this a *Domain Specific Language (DSL)*,
which we embed inside our general purpose *host language*.

There is a lot of overlap between creating DSLs and libraries.
The characteristic feature of using a DSL is that
we split our codebase into two halves:

1. Write a "program" using the DSL that
   represents a set of steps we want to run.

2. Run the program to an *interpreter*.
   This is typically a function that
   accepts a program as input and produces a useful result.

The programs in step 1 represent
the *structure* of the computation we want to run.
However, they don't do any work on their own.
To do work we have to run the program through an interpreter,
which provides implementations for each step
and handles underlying complexities and implementation details.

## Modularity and reuse

Splitting a codebase into programs and interpreters
offers a number of benefits in terms of
the modularity of our codebase:

- Our interpreter takes care of implementation details,
  making programs easier to write.
  We can re-use our DSL to reduce
  the overall complexity of our codebase.

- We can invent different interpreters
  to execute programs in different ways.
  Examples include: pretty printing expressions,
  estimating the length or runtime of a program,
  explaining a result as we calculate it,
  and performing on-the-fly optimisation and caching.
  We can write different interpreters
  that abstract over different effects:
  synchronous versus asynchronous computation,
  different kinds of error handling,
  interpreters optimised for unit testing,
  and so on.

- We can invent different syntaxes for writing programs.
  These syntaxes can be *standalone*,
  with complete parsers and external tooling,
  or *embedded* as libraries in our host language.
  Embedding allows us to re-use aspects of the host language
  such as variable definitions, types, and scoping rules.
  Embedding DSLs makes them feasible to implement,
  even as part of a commercial programming project
  with a tight deadline.

The general pattern looks like this:

```
Syntax 1 --\                /--> Interpreter 1 --> Result 1
            \      DSL     /
Syntax 2 ------> Program ------> Interpreter 2 --> Result 2
            /              \
Syntax 3 --/                \--> Interpreter 3 --> Result 3
```

## Inspecting and modifying programs

If we design our DSL carefully,
we open up options for inspecting and rewriting programs.
We can produce interpreters that consume one program
and output another, either in the same DSL or in another.
These interpreters are called *compilers*
(or "transpilers" in some circles):

```
Program 1 -----> Compiler -----> Program 2
```

## Constraints liberate, liberty constrains

Programs are easier to inspect and modify
if they are built from simple components.
The more flexible our DSL,
the harder it becomes to reason about.
As a [wise man once said](link-runar-constraints),
"constraints liberate, liberty constrains".

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
