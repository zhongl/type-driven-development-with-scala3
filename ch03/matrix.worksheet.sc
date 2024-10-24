import scala.annotation.tailrec
import scala.compiletime.constValue
import scala.compiletime.ops.int.S
import scala.compiletime.summonFrom

enum Vect[N <: Int, +A]:
  case `[]`                                                       extends Vect[0, Nothing]
  case ::[N <: Int, +A] private[Vect] (head: A, tail: Vect[N, A]) extends Vect[S[N], A]

  def ::[B >: A](b: B) = new ::(b, this)
end Vect

object Vect:
  extension [N <: Int, A](xs: Vect[N, A])
    @tailrec
    def seq(acc: Seq[A] = Seq()): Seq[A] = xs match
      case `[]`   => acc
      case h :: t => t.seq(acc :+ h)

    def show(using f: Show[Dim[A], Vect[N, A]]) = f(xs)

  type Dim[A] <: Int = A match
    case Vect[?, a] => S[Dim[a]]
    case Any        => 0

  opaque type Show[D <: Int, -A] = A => String
  given nil[D <: Int, A]: Show[D, Vect[0, A]]     = _ => "[]"
  given cons[N <: Int, A]: Show[0, Vect[S[N], A]] = _.seq().mkString("[", ", ", "]")
  given nest[D <: Int, N <: Int, A](using f: Show[D, A], t: Tab[S[D]]): Show[S[D], Vect[S[N], A]] =
    _.seq().map(f).mkString(s"${t.ac}[\n${t.de}", s"\n${t.de}", s"\n${t.ac}]")

  sealed trait Tab[N <: Int]:
    inline val ws = "  "
    def ac: String
    def de: String
  object Tab:
    given Tab[1] with
      def ac = ""
      def de = ws
    given [N <: Int](using n: Tab[N]): Tab[S[N]] with
      def ac = ws + n.ac
      def de = ws + n.de

  type Matrix[R <: Int, C <: Int, A] = Vect[R, Vect[C, A]]
  extension [R <: Int, C <: Int, A](mat: Matrix[R, C, A])
    def transpose(using Transpose[R, C, A]): Matrix[C, R, A] = Transpose(mat)

  opaque type Transpose[R <: Int, C <: Int, A] = Matrix[R, C, A] => Matrix[C, R, A]
  object Transpose:
    def apply[R <: Int, C <: Int, A](using f: Transpose[R, C, A]) = f
    given [C <: Int: Repeat, A]: Transpose[0, C, A]               = _ => Repeat(`[]`)
    given [R <: Int, C <: Int, A](using Transpose[R, C, A], Piece[R, C, A]): Transpose[S[R], C, A] =
      case x :: xs => Piece(x, Transpose(xs))
      case _       => ???

    opaque type Piece[R <: Int, C <: Int, A] = (Vect[C, A], Matrix[C, R, A]) => Matrix[C, S[R], A]
    object Piece:
      def apply[R <: Int, C <: Int, A](using f: Piece[R, C, A]) = f
      given [R <: Int, A]: Piece[R, 0, A]                       = (_, _) => `[]`
      given [R <: Int, C <: Int, A](using Piece[R, C, A]): Piece[R, S[C], A] =
        case (x :: xs, y :: ys) => (x :: y) :: Piece(xs, ys)
    end Piece
  end Transpose

  opaque type Repeat[N <: Int] = [A] => A => Vect[N, A]
  object Repeat:
    def apply[N <: Int, A](using f: Repeat[N])(a: A) = f(a)
    given Repeat[0]                                  = [A] => _ => `[]`
    given [N <: Int: Repeat]: Repeat[S[N]]           = [A] => a => a :: Repeat(a)
  end Repeat

end Vect

import Vect.*

`[]`.show

(1 :: `[]`).show

val mat: Matrix[3, 2, Int] =
  (1 :: 2 :: `[]`) ::
    (3 :: 4 :: `[]`) ::
    (5 :: 6 :: `[]`) ::
    `[]`

mat.show

mat.transpose.show
