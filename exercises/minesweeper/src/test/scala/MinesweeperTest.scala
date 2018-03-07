import org.scalatest.{Matchers, FunSuite}
import shapeless._
import syntax.sized._
import Minesweeper._
import Nat._

/** @version 1.1.0 */
class MinesweeperTest extends FunSuite with Matchers {

  test("no rows") {
    Minesweeper.annotate(Board.empty) should be(Board.empty)
  }

  test("no columns") {
    Minesweeper.annotate(Sized(Row.empty)) should be(Sized(Row.empty))
  }

  test("no mines") {
    Minesweeper.annotate(Sized(|.-.-.-.|,
                               |.-.-.-.|,
                               |.-.-.-.|)) should be(
      Sized(|.-.-.-.|,
            |.-.-.-.|,
            |.-.-.-.|))
  }

  test("minefield with only mines") {
    Minesweeper.annotate(Sized(|.*.*.*.|,
                               |.*.*.*.|,
                               |.*.*.*.|)) should be(
      Sized(|.*.*.*.|,
            |.*.*.*.|,
            |.*.*.*.|))
  }

  test("mine surrounded by spaces") {
    Minesweeper.annotate(Sized(|.-.-.-.|,
                               |.-.*.-.|,
                               |.-.-.-.|)) should be(
      Sized(|.`1`.`1`.`1`.|,
            |.`1`.`*`.`1`.|,
            |.`1`.`1`.`1`.|))
  }

  test("space surrounded by mines") {
    Minesweeper.annotate(Sized(|.*.*.*.|,
                               |.*.-.*.|,
                               |.*.*.*.|)) should be(
      Sized(|.`*`.`*`.`*`.|,
            |.`*`.`8`.`*`.|,
            |.`*`.`*`.`*`.|))
  }

  test("horizontal line") {
    Minesweeper.annotate(Sized(|.-.*.-.*.-.|)) should be(Sized(|.`1`.`*`.`2`.`*`.`1`.|))
  }

  test("horizontal line, mines at edges") {
    Minesweeper.annotate(Sized(|.*.-.-.-.*.|)) should be(Sized(|.`*`.`1`.`-`.`1`.`*`.|))
  }

  test("vertical line") {
    Minesweeper.annotate(Sized(|.-.|,
                               |.*.|,
                               |.-.|,
                               |.*.|,
                               |.-.|)) should be(
     Sized(|.`1`.|,
           |.`*`.|,
           |.`2`.|,
           |.`*`.|,
           |.`1`.|))
  }

  test("vertical line, mines at edges") {
    Minesweeper.annotate(Sized(|.*.|,
                               |.-.|,
                               |.-.|,
                               |.-.|,
                               |.*.|)) should be(
     Sized(|.`*`.|,
           |.`1`.|,
           |.`-`.|,
           |.`1`.|,
           |.`*`.|))
  }

  test("cross") {
    Minesweeper.annotate(Sized(|.-.-.*.-.-.|,
                               |.-.-.*.-.-.|,
                               |.*.*.*.*.*.|,
                               |.-.-.*.-.-.|,
                               |.-.-.*.-.-.|)) should be(
      Sized(|.`-`.`2`.`*`.`2`.`-`.|,
            |.`2`.`5`.`*`.`5`.`2`.|,
            |.`*`.`*`.`*`.`*`.`*`.|,
            |.`2`.`5`.`*`.`5`.`2`.|,
            |.`-`.`2`.`*`.`2`.`-`.|))
  }

  test("large minefield") {
    Minesweeper.annotate(Sized(|.-.*.-.-.*.-.|,
                               |.-.-.*.-.-.-.|,
                               |.-.-.-.-.*.-.|,
                               |.-.-.-.*.-.*.|,
                               |.-.*.-.-.*.-.|,
                               |.-.-.-.-.-.-.|)) should be(
      Sized(|.`1`.`*`.`2`.`2`.`*`.`1`.|,
            |.`1`.`2`.`*`.`3`.`2`.`2`.|,
            |.`-`.`1`.`2`.`3`.`*`.`2`.|,
            |.`1`.`1`.`2`.`*`.`4`.`*`.|,
            |.`1`.`*`.`2`.`2`.`*`.`2`.|,
            |.`1`.`1`.`1`.`1`.`1`.`1`.|))
  }
}
