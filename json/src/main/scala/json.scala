import shapeless._
import shapeless.labelled._

sealed abstract class Json {
  def stringify: String = Json.stringify(this)
}
final case class JsonObject(fields: List[(String, Json)]) extends Json
final case class JsonArray(items: List[Json])             extends Json
final case class JsonString(value: String)                extends Json
final case class JsonNumber(value: Double)                extends Json
final case class JsonBoolean(value: Boolean)              extends Json
case object JsonNull                                      extends Json

object Json {
  def encode[A](value: A)(implicit encoder: JsonEncoder[A]): Json =
    encoder.encode(value)

  def stringify(json: Json): String = json match {
    case JsonObject(fields) => "{" + fields.map(stringifyField).mkString(",") + "}"
    case JsonArray(items)   => "[" + items.map(stringify).mkString(",") + "]"
    case JsonString(value)  => "\"" + escape(value) + "\""
    case JsonNumber(value)  => value.toString
    case JsonBoolean(value) => value.toString
    case JsonNull           => "null"
  }

  private def stringifyField(field: (String, Json)): String = {
    val (name, value) = field
    escape(name) + ":" + stringify(value)
  }

  private def escape(str: String): String =
    "\"" + str.replaceAll("\"", "\\\\\"") + "\""
}

trait JsonEncoder[A] {
  def encode(value: A): Json
}

trait JsonObjectEncoder[A] extends JsonEncoder[A] {
  def encode(value: A): JsonObject
}

object JsonEncoder {
  def apply[T](
    implicit ev: JsonEncoder[T]
  ): JsonEncoder[T] = ev

  def pure[A](func: A => Json): JsonEncoder[A] =
    new JsonEncoder[A] {
      def encode(value: A): Json =
        func(value)
    }

  def pureObj[A](func: A => JsonObject): JsonObjectEncoder[A] =
    new JsonObjectEncoder[A] {
      def encode(value: A): JsonObject =
        func(value)
    }

  implicit val stringEnc: JsonEncoder[String] =
    pure(str => JsonString(str))

  implicit val intEnc: JsonEncoder[Int] =
    pure(num => JsonNumber(num))

  implicit val doubleEnc: JsonEncoder[Double] =
    pure(num => JsonNumber(num))

  implicit val booleanEnc: JsonEncoder[Boolean] =
    pure(bool => JsonBoolean(bool))

  implicit def listEnc[T](
    implicit tenc: JsonEncoder[T]
  ): JsonEncoder[List[T]] =
    pure(list => JsonArray(list.map(tenc.encode)))

  implicit def optEnc[T](
    implicit tenc: JsonEncoder[T]
  ): JsonEncoder[Option[T]] =
    pure(_.map(tenc.encode).getOrElse(JsonNull))

  implicit val hnilEnc: JsonObjectEncoder[HNil] =
    pureObj(hnil => JsonObject(Nil))

  implicit def hlistEnc[K <: Symbol, H, T <: HList](
    implicit
    witness: Witness.Aux[K],
    henc: Lazy[JsonEncoder[H]],
    tenc: JsonObjectEncoder[T],
  ): JsonObjectEncoder[FieldType[K, H] :: T] =
    pureObj(list => JsonObject(
        fields = (
          witness.value.name,
          henc.value.encode(list.head),
        ) :: tenc.encode(list.tail).fields
      )  
    )

  implicit val cnilEnc: JsonObjectEncoder[CNil] =
    pureObj(cnil => throw new IllegalStateException("Impossible"))

  implicit def coproductEnc[K <: Symbol, H, T <: Coproduct](
    implicit
    witness: Witness.Aux[K],
    henc: Lazy[JsonObjectEncoder[H]],
    tenc: JsonObjectEncoder[T],
  ): JsonObjectEncoder[FieldType[K, H] :+: T] =
    pureObj {
      case Inl(value) => JsonObject(
        fields = (
          "type",
          JsonString(witness.value.name),
        ) :: henc.value.encode(value).fields
      )
      case Inr(value) => tenc.encode(value)
    }

  implicit def genericEnc[A, R](
    implicit
    gen: LabelledGeneric.Aux[A, R],
    enc: JsonObjectEncoder[R],
  ): JsonObjectEncoder[A] = 
    pureObj(value => enc.encode(gen.to(value)))
}

final case class Employee(
  name: String,
  number: Int,
  manager: Boolean
)

final case class IceCream(
  name: String,
  numCherries: Int,
  inCone: Boolean
)

sealed trait Shape

final case class Rectangle(
  width: Double,
  height: Double
) extends Shape

final case class Circle(
  radius: Double
) extends Shape

object Main extends Demo {

  val employee1 = Employee("Alice", 1, true)
  val employee2 = Employee("Bob", 2, false)
  val employee3 = Employee("Charlie", 3, false)

  val iceCream1 = IceCream("Cornetto", 0, true)
  val iceCream2 = IceCream("Sundae", 1, false)

  val shape1: Shape = Rectangle(3, 4)
  val shape2: Shape = Circle(1)

  println(Json.encode(employee1).stringify)
  println(Json.encode(iceCream1).stringify)
  println(Json.encode(shape1).stringify)
  println(Json.encode(shape2).stringify)

}
