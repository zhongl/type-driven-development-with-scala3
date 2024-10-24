import scala.annotation.implicitNotFound
import scala.compiletime.ops.int.S

enum Vect[N <: Int, +A]:
  case `[]`                                                                  extends Vect[0, Nothing]
  case ::[N <: Int, +A, H <: A, T <: Vect[N, A]] private[Vect] (x: H, xs: T) extends Vect[S[N], A]

  def ::[X >: A](x: X) = new ::[N, X, x.type, this.type](x, this)

object Vect:
  extension [N <: Int, A, XS <: ::[N, A, ?, ?]](xs: XS)
    def -(x: A)(using f: Remove[N, A, x.type, XS]): Vect[N, A] = f(xs)

  @implicitNotFound("Can't remove element from the vect does't contain")
  opaque type Remove[N <: Int, A, X <: A, -XS <: Vect[S[N], A]] = XS => Vect[N, A]
  object Remove:
    given [A, N <: Int, X <: A]: Remove[N, A, X, ::[N, A, X, ?]] =
      case _ :: xs => xs

    given [A, N <: Int, X <: A, XS <: Vect[S[N], A], YS <: ::[S[N], A, ?, XS]](using
      f: Remove[N, A, X, XS]
    ): Remove[S[N], A, X, YS] =
      case x :: xs => x :: f(xs)

end Vect

import Vect.*

val v1 = 1 :: `[]`

v1 - 1

//v1 - 2

(3 :: 2 :: v1) - 1

("a" :: `[]`) - "a"
