import scala.annotation.tailrec
import scala.compiletime.ops.int.S

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
    case _          => 0

  opaque type Show[D <: Int, -A] = A => String
  object Show:
    given zero[D <: Int, A]: Show[D, Vect[0, A]]   = _ => "[]"
    given one[N <: Int, A]: Show[0, Vect[S[N], A]] = _.seq().mkString("[", ", ", "]")
    given more[D <: Int, N <: Int, A](using f: Show[D, A], t: Tab[S[D]]): Show[S[D], Vect[S[N], A]] =
      _.seq().map(f).mkString(s"${t.ac}[\n${t.de}", s"\n${t.de}", s"\n${t.ac}]")
  end Show

  final case class Tab[N <: Int](ac: String, de: String)
  object Tab:
    inline val ws                                = "  "
    given Tab[1]                                 = Tab("", ws)
    given [N <: Int](using n: Tab[N]): Tab[S[N]] = Tab(ws + n.ac, ws + n.de)

  type Mat[R <: Int, C <: Int, A] = Vect[R, Vect[C, A]]
  extension [R <: Int, C <: Int, A](mat: Mat[R, C, A])
    def trans(using f: Trans[R, C, A]): Mat[C, R, A] =
      f(mat)

  opaque type Trans[R <: Int, C <: Int, A] = Mat[R, C, A] => Mat[C, R, A]
  object Trans:
    given [C <: Int, A](using f: Fill[C]): Trans[0, C, A] = _ => f(`[]`)
    given [R <: Int, C <: Int, A](using f: Trans[R, C, A]): Trans[S[R], C, A] =
      case x :: xs => zip(x, f(xs))
      case ignore  => ???

    def zip[R <: Int, C <: Int, A]: (Vect[C, A], Mat[C, R, A]) => Mat[C, S[R], A] =
      case (`[]`, `[]`)       => `[]`
      case (x :: xs, y :: ys) => (x :: y) :: zip(xs, ys)
  end Trans

  opaque type Fill[N <: Int] = [A] => A => Vect[N, A]
  object Fill:
    given Fill[0]                                  = [A] => _ => `[]`
    given [N <: Int](using f: Fill[N]): Fill[S[N]] = [A] => a => a :: f(a)
  end Fill

end Vect

import Vect.*

`[]`.show

(1 :: `[]`).show

val mat: Mat[3, 2, Int] = (1 :: 2 :: `[]`)
  :: (3 :: 4 :: `[]`)
  :: (5 :: 6 :: `[]`) :: `[]`

mat.show

mat.trans.show
