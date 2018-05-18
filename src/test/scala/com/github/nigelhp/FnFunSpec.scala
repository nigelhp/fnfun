package com.github.nigelhp

import org.scalatest.{FreeSpec, Matchers}

class FnFunSpec extends FreeSpec with Matchers {

  private trait OneArgListFixture {
    /*
     * A simple two argument function: (String, Int) => String
     */
    def baseHttpUrl(host: String, port: Int): String =
      s"http://$host:$port"
  }

  "A simple 2-argument function" - {
    "can be applied to 2 arguments" in new OneArgListFixture {
      baseHttpUrl(host = "localhost", port = 8080) shouldBe "http://localhost:8080"

      /*
       * Using named parameters can improve readability - particularly useful in tests.
       */
    }

    "is itself a value" - {
      "which requires a trailing underscore in a context where a function is not expected" in new OneArgListFixture {
        /*
         * The trailing underscore "partially applies" the function (in this case with no arguments).
         */
        val fn = baseHttpUrl _
      }

      "which has the expected type" in new OneArgListFixture {
        /*
         * The underscore is not required in this case, as the type declaration creates a context that expects a function.
         */
        val fn: (String, Int) => String = baseHttpUrl
      }

      "which can be applied" - {
        "via () - which is itself a shorthand for invoking apply" in new OneArgListFixture {
          val fn = baseHttpUrl _

          fn("localhost", 8080) shouldBe "http://localhost:8080"
          fn.apply("localhost", 8080) shouldBe "http://localhost:8080"

          /*
           * This highlights that a function is actually a trait (or more accurately the series of traits Function1,
           * Function2, ... Function22), and so itself has methods.  We will come on to these shortly ...
           */

          /*
           * As a function value we cannot use named parameters.
           */
          // fn.apply(host = "localhost", port = 8080)                  // does not compile
        }
      }
    }

    "can be tupled in order to create a new function that accepts all arguments as a single tuple" in new OneArgListFixture {
      val hostAndPort = ("localhost", 8080)

      /*
       * What if I have the host and port as a tuple?
       * I could unpack the tuple and then apply the function to individual arguments.
       */
      val (host, port) = hostAndPort
      baseHttpUrl(host, port) shouldBe "http://localhost:8080"

      /* or the uglier */
      baseHttpUrl(hostAndPort._1, hostAndPort._2) shouldBe "http://localhost:8080"

      /*
       * Alternatively tupled (which is a method on the function) allows us to create a new function that
       * accepts 1-arg that is a pair / TupleN rather than n-args.
       */
      val fn = (baseHttpUrl _).tupled
      fn(hostAndPort) shouldBe "http://localhost:8080"

      /*
       * Now with the function signature to confirm exactly what has happened here:
       */
      val fnWithSignature: ((String, Int)) => String = (baseHttpUrl _).tupled
    }

    "can be partially applied" - {
      "with the first argument to create a new function" in new OneArgListFixture {
        val fn = baseHttpUrl("localhost", _: Int)

        fn(8080) shouldBe "http://localhost:8080"
        fn(80) shouldBe "http://localhost:80"

        /*
         * Now with the function signature to confirm exactly what happened here:
         */
        val fnWithSignature: Int => String = baseHttpUrl("localhost", _)
      }

      "with the second argument to create a new function" in new OneArgListFixture {
        val fn = baseHttpUrl(_: String, 8080)

        fn("localhost") shouldBe "http://localhost:8080"
        fn("theserver") shouldBe "http://theserver:8080"

        /*
         * Now with the function signature to confirm exactly what happened here:
         */
        val fnWithSignature: String => String = baseHttpUrl(_, 8080)
      }

      /*
       * Note that limitations of type inference dictate specifying the types of omitted arguments in this case.
       * This is not necessary with curried functions (covered later).
       */
    }

    "can be curried in order to create a new function that accepts one argument and returns a function that accepts the second argument" in new OneArgListFixture {
      val fn = (baseHttpUrl _).curried

      /*
       * Partially applying the function is then very natural - creating a new function that has a hostname 'embedded within it'.
       */
      val localhostWithPort = fn("localhost")

      localhostWithPort(8080) shouldBe "http://localhost:8080"
      localhostWithPort(80) shouldBe "http://localhost:80"

      /*
       * Now with the function signatures to confirm exactly what has happened here:
       */
      val fnWithSignature: String => Int => String = (baseHttpUrl _).curried
      val localhostWithPortWithSignature: Int => String = fnWithSignature("localhost")
    }
  }


  private trait TwoArgListFixture {
    /*
     * We can also have multiple argument lists - here we have two argument lists, each of one argument.
     */
    def baseHttpUrl(host: String)(port: Int): String =
      s"http://$host:$port"
  }

  "A function with multiple argument lists" - {
    "can be applied to multiple argument lists" in new TwoArgListFixture {
      baseHttpUrl(host = "localhost")(port = 8080) shouldBe "http://localhost:8080"
    }

    "is equivalent to the curried form when each argument list accepts only one argument" in new TwoArgListFixture {
      val fn: String => Int => String = baseHttpUrl

      fn("localhost")(8080) shouldBe "http://localhost:8080"
    }

    "but is actually a function that accepts the first argument list and returns a function that accepts the second argument list" in {
      /*
       * A function where each argument list has multiple arguments
       */
      def fn(str: String, int: Int)(c: Char, bi: BigInt): String = ???

      /*
       * has the signature:
       */
      val signature: (String, Int) => (Char, BigInt) => String = fn
    }

    "is most commonly used where implicits are involved" in {
      trait Writer[A] {
        def write(a: A): String
      }

      def fn[A](a: A)(implicit writer: Writer[A]): String = ???
    }

    "but is also how we can do dependency injection in a functional way without needing frameworks such as Guice" in {
      case class Enterprise(ern: String)
      def lookupEnterprise(host: String, port: Int)(ern: String): Option[Enterprise] = Some(Enterprise(ern))

      /*
       * The main / wiring code injects the config, returning a new function that can be passed to client code.
       */
      val configuredLookup: String => Option[Enterprise] = lookupEnterprise("localhost", 8080)

      /*
       * This can then be invoked by client code without it having to know anything about the dependencies.
       */
      configuredLookup("ENT123")
    }
  }


  private trait CompositionFixture {
    def increment(n: Int): Int =
      n + 1

    def square(n: Int): Int =
      n * n
  }

  "Functions can be composed" - {
    "via compose" in new CompositionFixture {
      /*
       * "compose" is the traditional FP function for composition:
       * (f compose g)(x) = f(g(x))
       */
      val incrementAndThenSquare = (square _).compose(increment)

      incrementAndThenSquare(3) shouldBe 16

      /*
       * Note that the function on which "compose" was called, is applied last.
       */
    }

    "via andThen" in new CompositionFixture {
      /*
       * (f andThen g)(x) = g(f(x))
       */
      val squareAndThenIncrement = (square _).andThen(increment)

      squareAndThenIncrement(3) shouldBe 10

      /*
       * The function on which "andThen" was called, is applied first.
       * This can be more readable as the function applications flow in the same left-to-right order as written.
       */
    }

    "which enables 'map fusion'" in new CompositionFixture {
      val numbers = Seq(1, 2, 3, 4, 5)

      /*
       * Two map operations
       */
      numbers.map(increment).map(square) should contain theSameElementsInOrderAs Seq(4, 9, 16, 25, 36)

      /*
       * can become one.
       */
      numbers.map((increment _).andThen(square)) should contain theSameElementsInOrderAs Seq(4, 9, 16, 25, 36)

      /*
       * This can be particularly beneficial when working with Futures.
       */
    }
  }
}
