import shapeless._

trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

object CsvEncoder {
  def pure[A](func: A => List[String]): CsvEncoder[A] =
    new CsvEncoder[A] {
      def encode(value: A): List[String] =
        func(value)
    }

  implicit val stringEnc: CsvEncoder[String] =
    pure(str => List(str))

  implicit val intEnc: CsvEncoder[Int] =
    pure(num => List(num.toString))

  implicit val booleanEnc: CsvEncoder[Boolean] =
    pure(bool => List(if(bool) "yes" else "no"))

  implicit val hnilEncoder: CsvEncoder[HNil] =
    pure(_ => Nil)

  implicit def hconsEncoder[H, T <: HList](
    // Lazy is workaround to supress compiler
    // defensive heuristics when resolving implicits
    implicit he: Lazy[CsvEncoder[H]], te: CsvEncoder[T]
  ): CsvEncoder[H :: T] = pure(hlist =>
    he.value.encode(hlist.head) ++ te.encode(hlist.tail)  
  )

  implicit val cnilEncoder: CsvEncoder[CNil] =
    pure(_ => throw new IllegalStateException("Impossible"))

  implicit def coprodEncoder[H, T <: Coproduct](
    implicit he: Lazy[CsvEncoder[H]], te: CsvEncoder[T]
  ): CsvEncoder[H :+: T] = pure {
    case Inl(h) => he.value.encode(h)
    case Inr(t) => te.encode(t)
  }

  implicit def genEncoder[A, R](
    implicit gen: Generic.Aux[A, R], enc: Lazy[CsvEncoder[R]]
  ): CsvEncoder[A] = pure(enc.value.encode _ compose gen.to _)
}

object Main extends Demo {
  def encodeCsv[A](value: A)(implicit enc: CsvEncoder[A]): List[String] =
    enc.encode(value)

  final case class TestProd(one: String, two: Int, three: Boolean)

  println(encodeCsv(TestProd("1", 2, false)))

  sealed trait TestCoprod
  final case class TestCoprod1(one: String) extends TestCoprod
  final case class TestCoprod2(two: Boolean) extends TestCoprod

  println(encodeCsv(TestCoprod1("test")))
  println(encodeCsv(TestCoprod2(true)))

  sealed trait Tree
  final case object Leaf extends Tree
  final case class Branch(value: Int, left: Tree, right: Tree) extends Tree

  // Fails to compile without Lazy
  // Because compiler encounters infinite loop
  println(encodeCsv(
    Branch(
      1,
      Branch(2, Leaf, Leaf),
      Branch(3, Branch(4, Leaf, Leaf), Leaf)
    )
  ))
}
