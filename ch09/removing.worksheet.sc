import scala.annotation.implicitNotFound
import scala.compiletime.ops.int.S

enum Vect[N <: Int, +A]:
  case `[]`                                                                    extends Vect[0, Nothing]
  case ::[N <: Int, +A, +H <: A, +T <: Vect[N, A]] private[Vect] (x: H, xs: T) extends Vect[S[N], A]

  def ::[X >: A](x: X) = new ::[N, X, x.type, this.type](x, this)

object Vect:
  extension [N <: Int, A, XXS <: Vect[S[N], A], XS <: Vect[N, A]](xs: XXS)
    def -(x: A)(using f: Remove[N, x.type, XXS, XS]): XS = f(xs)

  @implicitNotFound("Can't remove element from the vect does't contain")
  opaque type Remove[N <: Int, X, -XXS <: Vect[S[N], ?], +XS <: Vect[N, ?]] = XXS => XS
  object Remove:
    given here[A, N <: Int, X <: A, XS <: Vect[N, A]]: Remove[N, X, ::[N, A, X, XS], XS] =
      case _ :: xs => xs

    given threre[A, N <: Int, X <: A, Y <: A, XS <: Vect[N, A], YS <: Vect[S[N], A]](using
      f: Remove[N, X, YS, XS]
    ): Remove[S[N], X, ::[S[N], A, Y, YS], ::[N, A, Y, XS]] =
      case x :: xs => x :: f(xs)
  end Remove

end Vect

import Vect.*

val v1 = 2 :: 1 :: `[]`

v1 - 2 - 1

v1 - 1 // - 2 // error

val v2 = 3 :: v1

v2 - 1

("a" :: `[]`) - "a"
