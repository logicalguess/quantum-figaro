package quantum

import com.cra.figaro.algorithm.Values
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language._
import com.cra.figaro.library.compound.{CPD, ^^}
import org.scalatest.flatspec.AnyFlatSpec

class QSpec extends AnyFlatSpec {

  "one qubit" should "work" in {
    Universe.createNew()
    val q = Select(1.0 -> "0", 0.0 -> "1")
    val q1 = CPD(q, "0" -> Select(0.5 -> "0", 0.5 -> "1"), "1" -> Select(0.5 -> "0", 0.5 -> "1"))
    val c = Chain(q1, (w: String) => w match {
      case "0" => Select(0.2 -> "0", 0.8 -> "1")
      case "1" => Select(0.3 -> "0", 0.7 -> "1")
    })

    val alg = VariableElimination(c)
    alg.start()
    println("probability of 0: " + alg.probability(c, "0"))
    println("probability of 1: " + alg.probability(c, "1"))
    alg.stop()
  }

  "two qubits" should "work" in {
    Universe.createNew()

    def histogram(a00: Double, a01: Double, a10: Double, a11: Double) = {
      Select(a00 -> "00", a01 -> "01", a10 -> "10", a11 -> "11")
    }

    Universe.createNew()
    val q = Select(1.0 -> "00", 0.0 -> "01", 0.0 -> "10", 0.0 -> "11")
    //val q = Select(1.0 -> outcomes(0), 0.0 -> outcomes(0), 0.0 -> outcomes(0), 0.0 -> outcomes(0))

    val c = Chain(q, (s: String) => s match {
      case "00" => histogram(0.2, 0.3, 0.4, 0.1)
      case "01" => histogram(0.1, 0.4, 0.3, 0.2)
      case "10" => histogram(0.2, 0.3, 0.3, 0.2)
      case "11" => histogram(0.2, 0.3, 0.3, 0.2)
    })

    val alg = VariableElimination(c)
    alg.start()
    for (s <- Values()(q)) {
      println("probability of " + s + ": " + alg.probability(c, s))
    }
    alg.stop()
  }

  "Fibonacci numbers" should "sample" in {
    Universe.createNew()
    val q = Select(0.5 -> "0", 0.5 -> "1")

    val counts = collection.mutable.Map[String, Int]()
    for (_ <- 0 until 100) {
      q.generate()
      val k = q.value
      counts.update(k, counts.getOrElse(k, 0) + 1)
    }

    println(counts)
  }

  "Fibonacci numbers" should "chain" in {
    def fib(n: Int): Int = {
      val qs = new Array[Element[String]](n)
      qs(0) = Select(0.5 -> "0", 0.5 -> "1")

      for (i <- 1 until qs.length) {
        qs(i) = Chain(qs(i - 1), (s: String) =>
          if (s == "0") Select(0.5 -> "0", 0.5 -> "1")
          else Constant("0")
        )
      }

      val counts = collection.mutable.Map[Any, Int]()
      for (_ <- 0 until math.pow(2, n + 2).toInt) {
        for (i <- 0 until qs.length)
          qs(i).generate()

        val k = qs.map(_.value).mkString("")
        counts.update(k, counts.getOrElse(k, 0) + 1)
      }
      //println(counts)
      counts.size
    }

    for (n <- 1 to 10) {
      println(s"F($n) = ${fib(n)}")
    }
  }

  def noConsecutiveOnes(pair: (String, String)): Double = {
    if (pair._1 == "1" && pair._2 == "1") 0.0
    else 1.0
  }

  "Fibonacci numbers" should "ve" in {
    val n = 3

    val qs = Array.fill[Element[String]](n)(Select(0.5 -> "0", 0.5 -> "1"))

    for (i <- 0 until n - 1) {
      val pair = ^^(qs(i), qs(i + 1))
      pair.addConstraint(noConsecutiveOnes)
    }
    val alg = VariableElimination(qs: _*)
    alg.start()
    for (i <- 0 until n)
      println("Probability of 0 in position " + i + ": " + alg.probability(qs(i), "0"))
    alg.stop()
  }

  "Fibonacci numbers" should "constraints" in {
    def fib(n: Int): Int = {

      val counts = collection.mutable.Map[Any, Int]()
      for (_ <- 0 until math.pow(2, n + 2).toInt) {
        val qs = Array.fill[Element[String]](n)(Select(0.5 -> "0", 0.5 -> "1"))

        //        val c = RichCPD(qs(0), qs(1), qs(2), (*, *, *) -> Constant(0))

        for (i <- 0 until n - 1) {
          val pair = ^^(qs(i), qs(i + 1))
          pair.addConstraint(noConsecutiveOnes)
        }

        for (i <- 0 until n)
          qs(i).generate()

        //        c.generate()
        //        println(c.parent.value)

        var k = ""
        for (i <- 0 until n) {
          k += qs(i).value
        }
        counts.update(k, counts.getOrElse(k, 0) + 1)
      }

      println(counts)
      counts.size
    }

    for (n <- 3 to 3) {
      println(s"F($n) = ${fib(n)}")
    }
  }


  "Fibonacci numbers" should "sampling" in {
    def fib(n: Int): Int = {
      val c = nco(n)

      val alg = Importance(math.pow(2, n + 5).toInt, c)
      alg.start()
//      alg.distribution(c).print("\n")
//      println()
//      val p = alg.probability(c, (s: String) => s.contains(("11")))
      alg.distribution(c).filter((p => p._1 > 0)).size
      //alg.stop()
    }

    for (n <- 1 to 5) {
      println(s"F($n) -> ${fib(n)}")
    }
  }

  "Fibonacci numbers" should "generate" in {
    def fib(n: Int): Int = {
      val counts = collection.mutable.Map[Any, Int]()
      for (_ <- 0 until math.pow(2, n + 2).toInt) {
        val c = nco(n)
        val alg = Importance(1, c)
        alg.start()
        val k = c.value
        print(alg.distribution(c))
        alg.stop()
        counts.update(k, counts.getOrElse(k, 0) + 1)
      }
      //println(counts)
      counts.size
    }

    for (n <- 1 to 10) {
      println(s"F($n) = ${fib(n)}")
    }
  }


  private def nco(n: Int) = {
    val qs = Array.fill[Element[String]](n)(Select(0.5 -> "0", 0.5 -> "1"))

    for (i <- 0 until n - 1) {
      val pair = ^^(qs(i), qs(i + 1))
      pair.addConstraint(noConsecutiveOnes)
    }

    Apply(Inject(qs: _*), (v: List[String]) => v.mkString(""))
  }
}
