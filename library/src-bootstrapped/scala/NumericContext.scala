package scala

import quoted.{Expr, QuoteContext, Type, Liftable}
import quoted.matching.Const

import util.FromDigits

case object NumericContext
  val fb: FromDigits.BigDecimalFromDigits.type = FromDigits.BigDecimalFromDigits
  val ib: FromDigits.BigIntFromDigits.type     = FromDigits.BigIntFromDigits
  val i8: ByteFromDigits.type                  = ByteFromDigits
  val i16: ShortFromDigits.type                = ShortFromDigits
  val u16: CharFromDigits.type                 = CharFromDigits
  val i32: IntFromDigits.type                  = IntFromDigits
  val i64: LongFromDigits.type                 = LongFromDigits

  type NumberType[U <: FromDigits[?]] = U match
    case FromDigits[t] => t

  object ByteFromDigits extends IntegralFromDigits[Byte]
    override inline def fromDigits(digits: String, radix: Int): Byte = ${ fromDigitsImpl[Byte]('digits, 'radix)   }
    override inline def fromDigits(digits: String): Byte             = ${ fromDigitsImpl[Byte]('digits, Expr(10)) }

  object ShortFromDigits extends IntegralFromDigits[Short]
    override inline def fromDigits(digits: String, radix: Int): Short = ${ fromDigitsImpl[Short]('digits, 'radix)   }
    override inline def fromDigits(digits: String): Short             = ${ fromDigitsImpl[Short]('digits, Expr(10)) }

  object CharFromDigits extends IntegralFromDigits[Char]
    override inline def fromDigits(digits: String, radix: Int): Char = ${ fromDigitsImpl[Char]('digits, 'radix)   }
    override inline def fromDigits(digits: String): Char             = ${ fromDigitsImpl[Char]('digits, Expr(10)) }

  object IntFromDigits extends IntegralFromDigits[Int]
    override inline def fromDigits(digits: String, radix: Int): Int = ${ fromDigitsImpl[Int]('digits, 'radix)   }
    override inline def fromDigits(digits: String): Int             = ${ fromDigitsImpl[Int]('digits, Expr(10)) }

  object LongFromDigits extends IntegralFromDigits[Long]
    override inline def fromDigits(digits: String, radix: Int): Long = ${ fromDigitsImpl[Long]('digits, 'radix)   }
    override inline def fromDigits(digits: String): Long             = ${ fromDigitsImpl[Long]('digits, Expr(10)) }

  private def fromDigitsImpl[T: Liftable](digits: Expr[String], radix: Expr[Int])(
    given qctx: QuoteContext, parser: Parser[T]
  ): Expr[T] =
    val isDigits = Const.unapply(digits)
    val isRadix  = Const.unapply(radix)
    isDigits -> isRadix match
    case Some(d) -> Some(r) =>
      try parser.fromLong(FromDigits.longFromDigits(d, r)) match
        case Some(value) => Expr(value)
        case None        => qctx.error("number too large", digits); Expr(parser.zero)
      catch
        case e: FromDigits.FromDigitsException => qctx.error(e.getMessage, digits); Expr(parser.zero)
    case _ =>
      if isDigits.isEmpty then
        qctx.error("Non-constant digits.", digits)
      else
        qctx.error("Non-constant radix.", radix)
      Expr(parser.zero)

  private sealed trait Parser[T]
    def zero: T
    def fromLong(l: Long): Option[T]

  private given Parser[Byte]
    def zero = 0
    def fromLong(l: Long) = Option.when(l.isValidByte)(l.toByte)

  private given Parser[Short]
    def zero = 0
    def fromLong(l: Long) = Option.when(l.isValidShort)(l.toShort)

  private given Parser[Char]
    def zero = 0
    def fromLong(l: Long) = Option.when(l.isValidChar)(l.toChar)

  private given Parser[Int]
    def zero = 0
    def fromLong(l: Long) = Option.when(l.isValidInt)(l.toInt)

  private given Parser[Long]
    def zero = 0
    def fromLong(l: Long) = Some(l)

  private[NumericContext] sealed class IntegralFromDigits[T] extends FromDigits.WithRadix[T]
    def fromDigits(digits: String, radix: Int): T = sys.error("un-inlined call to fromDigits.")
