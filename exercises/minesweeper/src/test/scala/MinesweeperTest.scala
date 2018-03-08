import org.scalatest.{Matchers, FunSuite}
import shapeless._
import syntax.sized._
import Minesweeper._
import Nat._
import vec._
import MinesweeperDomain._

/** @version 1.1.0 */
class MinesweeperTest extends FunSuite with Matchers {

  test("no rows") {
    Minesweeper.annotate(IBoard.empty) should be(OBoard.empty)
  }

  test("no columns") {
    Minesweeper.annotate(Vec(IRow.empty)) should be(Vec(ORow.empty))
  }

  test("no mines") {
    Minesweeper.annotate(Vec(|.__.__.__.|,
                             |.__.__.__.|,
                             |.__.__.__.|)) should be(
      Vec(|.__.__.__.|,
          |.__.__.__.|,
          |.__.__.__.|))
  }

  test("minefield with only mines") {
    Minesweeper.annotate(Vec(|._M._M._M.|,
                             |._M._M._M.|,
                             |._M._M._M.|)) should be(
      Vec(|._M._M._M.|,
          |._M._M._M.|,
          |._M._M._M.|))
  }

  test("mine surrounded by spaces") {
    Minesweeper.annotate(Vec(|.__.__.__.|,
                             |.__._M.__.|,
                             |.__.__.__.|)) should be(
      Vec(|._1._1._1.|,
          |._1._M._1.|,
          |._1._1._1.|))
  }

  test("space surrounded by mines") {
    Minesweeper.annotate(Vec(|._M._M._M.|,
                             |._M.__._M.|,
                             |._M._M._M.|)) should be(
      Vec(|._M._M._M.|,
          |._M._8._M.|,
          |._M._M._M.|))
  }

  test("horizontal line") {
    Minesweeper.annotate(Vec(|.__._M.__._M.__.|)) should be(Vec(|._1._M._2._M._1.|))
  }

  test("horizontal line, mines at edges") {
    Minesweeper.annotate(Vec(|._M.__.__.__._M.|)) should be(Vec(|._M._1.__._1._M.|))
  }

  test("vertical line") {
    Minesweeper.annotate(Vec(|.__.|,
                             |._M.|,
                             |.__.|,
                             |._M.|,
                             |.__.|)) should be(
     Vec(|._1.|,
         |._M.|,
         |._2.|,
         |._M.|,
         |._1.|))
  }

  test("vertical line, mines at edges") {
    Minesweeper.annotate(Vec(|._M.|,
                             |.__.|,
                             |.__.|,
                             |.__.|,
                             |._M.|)) should be(
     Vec(|._M.|,
         |._1.|,
         |.__.|,
         |._1.|,
         |._M.|))
  }

  test("cross") {
    Minesweeper.annotate(Vec(|.__.__._M.__.__.|,
                             |.__.__._M.__.__.|,
                             |._M._M._M._M._M.|,
                             |.__.__._M.__.__.|,
                             |.__.__._M.__.__.|)) should be(
      Vec(|.__._2._M._2.__.|,
          |._2._5._M._5._2.|,
          |._M._M._M._M._M.|,
          |._2._5._M._5._2.|,
          |.__._2._M._2.__.|))
  }

  test("large minefield") {
    Minesweeper.annotate(Vec(|.__._M.__.__._M.__.|,
                             |.__.__._M.__.__.__.|,
                             |.__.__.__.__._M.__.|,
                             |.__.__.__._M.__._M.|,
                             |.__._M.__.__._M.__.|,
                             |.__.__.__.__.__.__.|)) should be(
      Vec(|._1._M._2._2._M._1.|,
          |._1._2._M._3._2._2.|,
          |.__._1._2._3._M._2.|,
          |._1._1._2._M._4._M.|,
          |._1._M._2._2._M._2.|,
          |._1._1._1._1._1._1.|))
  }
}

class RowBuilder[T <: OutputBoardCell, N <: Nat](row: Vec[N, T]) {

  type O = OutputBoardCell
  type I = InputBoardCell

  def asO(r: Vec[N, O]): Vec[N, O] = r.asInstanceOf[Vec[N, O]]
  def asT(r: Vec[N, T]): Vec[N, T] = r

  def asO(r: O): O = r
  def asT(r: O): T = r.asInstanceOf[T]
  def asT(r: I): T = r.asInstanceOf[T]

  def __  = new RowBuilder(asT(row) :+ asT(BoardCell.__))
  def _M  = new RowBuilder(asT(row) :+ asT(BoardCell._M))
  def _1  = new RowBuilder(asO(row) :+ asO(BoardCell._1))
  def _2  = new RowBuilder(asO(row) :+ asO(BoardCell._2))
  def _3  = new RowBuilder(asO(row) :+ asO(BoardCell._3))
  def _4  = new RowBuilder(asO(row) :+ asO(BoardCell._4))
  def _5  = new RowBuilder(asO(row) :+ asO(BoardCell._5))
  def _6  = new RowBuilder(asO(row) :+ asO(BoardCell._6))
  def _7  = new RowBuilder(asO(row) :+ asO(BoardCell._7))
  def _8  = new RowBuilder(asO(row) :+ asO(BoardCell._8))

  def | = row
}

object | extends RowBuilder[InputBoardCell, _0](IRow.empty)


