package quantum

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language.{Chain, Constant, Element, Select, Universe}
import com.cra.figaro.library.compound.{*, CPD, RichCPD, ^^}
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
    def histogram(a00: Double, a01: Double, a10: Double, a11: Double) = {
      Select(a00 -> "00", a01 -> "01", a10 -> "10", a11 -> "11")
    }

    val outcomes = List("00", "01", "10", "11")
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
    for (s <- outcomes) {
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

}
