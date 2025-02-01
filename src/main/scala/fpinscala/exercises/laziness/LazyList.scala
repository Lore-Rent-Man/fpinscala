package fpinscala.exercises.laziness

import LazyList.*

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = 
    @annotation.tailrec
    def go(xs: LazyList[A], a: List[A]): List[A] = 
      xs match
        case Empty => a.reverse
        case Cons(h, t) => go(t(), h() :: a)
    go(this, Nil)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty

  @annotation.tailrec
  final def drop(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => this

  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = 
    foldRight(None: Option[A])((a, b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] = 
    foldRight(Empty: LazyList[B])((a, b) => cons(f(a), b))
  
  def filter(f: A => Boolean): LazyList[A] = 
    foldRight(Empty: LazyList[A])((a, b) => if f(a) then cons(a, b) else b)

  def append[A2 >: A](that: => LazyList[A2]): LazyList[A2] = 
    foldRight(that)((a, b) => cons(a, b))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] = 
    foldRight(Empty: LazyList[B])((a, b) => f(a).append(b))

  def mapViaUnfold[B](f: A => B): LazyList[B] = 
    unfold(this):
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
  
  def takeViaUnfold(n: Int): LazyList[A] = 
    unfold((this, n)):
      case (Cons(h, t), nn) if nn > 0 => Some((h(), (t(), nn - 1)))
      case _ => None
  
  def takeWhileViaUnfold(f: A => Boolean): LazyList[A] = 
    unfold(this):
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
  
  def zipWith[B, C](xs: LazyList[B])(f: (A, B) => C): LazyList[C] = 
    unfold((this, xs)):
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(h, t), Cons(h2, t2)) => Some((f(h(), h2()), (t(), t2())))
  
  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] = 
    unfold((this, that)):
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
      case (Cons(h, t), Cons(h2, t2)) => Some((Some(h()), Some(h2())), (t(), t2()))

  def startsWith[B >: A](s: LazyList[B]): Boolean = 
    s match
      case Empty => true
      case _ => !zipAll(s).exists((a1, a2) => (a1, a2) match
        case (None, Some(_)) => true
        case (Some(aa), Some(bb)) => aa != bb
        case _ => false
      )
  
  def tails: LazyList[LazyList[A]] = 
    unfold(this):
      case Cons(h, t) => Some(cons(h(), t()), t())
      case Empty => None
    .append(LazyList(Empty))
  
  def hasSubsequence[A](l: LazyList[A]): Boolean =
    tails.exists(_.startsWith(l))
  
  def scanRight[B](z: => B)(f: (A, => B) => B): LazyList[B] = 
    foldRight((z, LazyList(z)))((a, b) => 
      lazy val bb = b
      val bb2 = f(a, bb(0))
      (bb2, cons(bb2, bb(1)))
    )._2


object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  lazy val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = 
    lazy val as: LazyList[A] = LazyList.cons(a, as)
    as

  def from(n: Int): LazyList[Int] = 
    val ns: LazyList[Int] = LazyList.cons(n, from(n + 1))
    ns

  lazy val fibs: LazyList[Int] =
    def go(current: Int, next: Int): LazyList[Int] =
      cons(current, go(next, current + next))
    go(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = 
    f(state) match
      case None => Empty
      case Some((a, s)) => cons(a, unfold(s)(f))

  lazy val fibsViaUnfold: LazyList[Int] = unfold((0, 1))(
    p => p match
      case (c, n) => Some((c, (n, n + c)))    
  )

  def fromViaUnfold(n: Int): LazyList[Int] = 
    unfold(n)(n => Some((n, n + 1)))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(a)(a => Some((a, a)))

  lazy val onesViaUnfold: LazyList[Int] = 
    continuallyViaUnfold(1)
