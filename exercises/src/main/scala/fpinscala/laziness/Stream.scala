package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h, t) if n == 1 => cons(h(), Empty)
    case _ => Empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def forAll(p: A => Boolean): Boolean =
    this.foldRight(true)( (a, b) => p(a) && b)

  def takeWhile1(p: A => Boolean): Stream[A] =
    this.foldRight(Empty: Stream[A])( (a, b) => if (p(a)) cons(a, b) else Empty )

  def headOption: Option[A] =
    this.foldRight(None: Option[A])( (a, _) => Some(a) )

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    this.foldRight(empty[B])( (h, t) => cons(f(h), t) )

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])( (h, t) => if (f(h)) cons(h, t) else t)

  def append[B >: A](b: => Stream[B]): Stream[B] =
    foldRight(b)( (h, t) => cons(h, t) )

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])( (h, t) => f(h).append(t) )

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case Empty => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), m) if m > 0 => Some((h(), (t(), m-1)))
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def zipWithViaUnfold[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, b)) {
    case ((Cons(ha, ta), Cons(hb, tb))) => Some((f(ha(), hb()), (ta(), tb())))
    case _ => None
  }

  def zipAll[B](b: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, b)) {
    case ((Cons(ha, ta), Cons(hb, tb))) => Some(((Some(ha()), Some(hb())), (ta(), tb())))
    case ((Cons(ha, ta), Empty)) => Some(((Some(ha()), None), (ta(), Empty)))
    case ((Empty, Cons(hb, tb))) => Some(((None, Some(hb())), (Empty, tb())))
    case _ => None
  }

  def startsWith[B](s: Stream[B]): Boolean = {
    zipAll(s).takeWhile(!_._2.isEmpty).forAll {
      case (a, b) => a == b
    }
  }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some(s, s drop 1)
    } append Empty

  def scanRight[B](z: B)(f: (A, =>B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val f: Stream[A] = Cons(()=>a, ()=>f)
    f
  }

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = cons(a, go(b, a+b))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => Empty
  }

  val fibs1 = unfold((0,1)){ case (a, b) => Some((a, (b, a+b))) }

  def constant1[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a,a)))

  def from1(n: Int): Stream[Int] =
    unfold(n)(s => Some((s ,s+1)))

  val ones1 = unfold(1)(_ => Some((1, 1)))


}