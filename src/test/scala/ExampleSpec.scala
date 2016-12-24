import il.co.codeguru.extreme.Main
import org.scalatest.FunSuite

class ExampleSpec extends FunSuite {

  test("check covered method") {
    val main = new Main()
    assert(main.coveredMethod())
  }
}