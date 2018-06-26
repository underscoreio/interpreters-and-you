# Interpreters and You

Slides, code samples, and notes on building programs with the interpreter pattern in Scala.

Copyright 2018 Dave Gurnell.

Text, diagrams, and slides licensed [CC-BY-SA 4.0][text-license].
Code samples licensed [Apache 2.0][code-license]

[text-license]: https://creativecommons.org/licenses/by-sa/4.0/
[code-license]: https://www.apache.org/licenses/LICENSE-2.0.html

## Content

The `/slides` directory contains the slides used in my presentation at Scala Days 2018.

- [Scala Days 2018 Berlin - Dave](https://github.com/underscoreio/interpreters-and-you/blob/master/slides/interpreters-and-you.key)
- [Scala Days 2018 New York - Mark](https://github.com/mmynsted/interpreters-and-you/blob/master/slides/interpreters-and-you.key)

The `/code` directory contains different sample interpreters, implemented in reified and Church encoded styles.

The `/text` directory contains a short guide to writing interpreters in Scala. This is currently a work-in-progress that I hope to flesh out to cover the same content as the talk.

## Building the Guide

Install Docker and use `go.sh` to boot an instance with most of the right dependencies:

~~~
bash$ cd text

bash$ ./go.sh
~~~

Use `yarn` to install the remaining dependencies:

~~~
docker$ npm intall -g yarn

docker$ yarn
~~~

Finally use `sbt` to build the guide. Choose one of the following commands:

~~~
docker$ sbt pdf

docker$ sbt html

docker$ sbt epub

docker$ sbt all
~~~
