import scala.annotation.tailrec
import scala.compiletime.constValue
import scala.compiletime.ops.int.{-, S}
import scala.compiletime.summonFrom

enum Vect[N <: Int, +A]:
  case `[]`                                                       extends Vect[0, Nothing]
  case ::[N <: Int, +A] private[Vect] (head: A, tail: Vect[N, A]) extends Vect[S[N], A]

  def ::[B >: A](b: B) = new ::(b, this)
end Vect

object Vect:

  type Dim[A] <: Int = A match
    case Vect[?, a] => S[Dim[a]]
    case _          => 0

  trait Show[-A]:
    def apply(a: A): String
  object Show:
    def apply[A](a: A)(using f: Show[A]) = f(a)

  given nil[A]: Show[Vect[0, A]] with
    def apply(a: Vect[0, A]): String = "[]"
  given cons[N <: Int, A](using MkString[Dim[A], A]): Show[Vect[N, A]] with
    def apply(a: Vect[N, A]): String =
      @tailrec
      def rec(xs: Vect[?, A], acc: Seq[A]): String = xs match
        case `[]`   => MkString(acc)
        case h :: t => rec(t, acc :+ h)

      rec(a, Seq.empty)

  opaque type MkString[I <: Int, A] = Seq[A] => String
  object MkString:
    def apply[I <: Int, A](using f: MkString[I, A]) = f

    inline val tab = "  "

    inline given [I <: Int, A]: MkString[I, A] = inline constValue[I] match
      case 0 => _.mkString("[", ", ", "]")
      case n => indent[I, A](tab * (n - 1), tab * n)

    inline def indent[I <: Int, A](ac: String, de: String): MkString[I, A] = summonFrom:
      case _: Show[A] => _.map(Show[A]).mkString(s"$ac[\n$de", s"\n$de", s"\n$ac]")
  end MkString

  opaque type Repeat[N <: Int] = [A] => A => Vect[N, A]
  object Repeat:
    def apply[N <: Int, A](using f: Repeat[N])(a: A) = f(a)
    given Repeat[0]                                  = [A] => _ => `[]`
    given [N <: Int: Repeat]: Repeat[S[N]]           = [A] => a => a :: Repeat(a)
  end Repeat

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

  extension [N <: Int, A](xs: Vect[N, A]) def show(using Show[Vect[N, A]]) = Show(xs)

  type Matrix[R <: Int, C <: Int, A] = Vect[R, Vect[C, A]]
  extension [R <: Int, C <: Int, A](mat: Matrix[R, C, A])
    def transpose(using Transpose[R, C, A]): Matrix[C, R, A] = Transpose(mat)
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
