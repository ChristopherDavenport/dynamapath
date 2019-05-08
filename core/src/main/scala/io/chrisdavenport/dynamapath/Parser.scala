package io.chrisdavenport.dynamapath

import atto._
import Atto._
import cats.data._
import cats.implicits._
import cats.Alternative
import scala.annotation.tailrec

object Parser {

  sealed trait PathSegment extends Product with Serializable
  // Constants
  final case class Constant(value: String) extends PathSegment
  // Params
  final case class Variable(name: String) extends PathSegment
  final case class OptVariable(name: String) extends PathSegment
  final case class ZeroOrMoreVariable(name: String) extends PathSegment
  final case class OneOrMoreVariable(name: String) extends PathSegment

  final case class Path(path: List[PathSegment]) extends AnyVal

  private val slash = '/'
  private val SLASH = char(slash).named("Slash")

  private val nonKeySegmentChars = letterOrDigit | char('-')
  private val variableIndicator = char(':')
  private val optionalIndicator = char('?')
  private val zeroOrMoreIndicator = char('*')
  private val oneOrMoreIndicator = char('+')

  val PATH_SEGMENT : Parser[PathSegment] =
    (variableIndicator ~> stringOf(nonKeySegmentChars) <~ optionalIndicator)
      .map(OptVariable)
      .widen[PathSegment] |
    (variableIndicator ~> stringOf(nonKeySegmentChars) <~ zeroOrMoreIndicator)
      .map(ZeroOrMoreVariable)
      .widen[PathSegment] |
    (variableIndicator ~> stringOf(nonKeySegmentChars) <~ oneOrMoreIndicator)
      .map(OneOrMoreVariable)
      .widen[PathSegment] |
    (variableIndicator ~> stringOf(nonKeySegmentChars))
      .map(Variable)
      .widen[PathSegment] |
    stringOf(nonKeySegmentChars).map(Constant).widen[PathSegment]

  val PATH =
    many(SLASH ~> PATH_SEGMENT)
    .map(Path(_))

  sealed trait PathValue extends Product with Serializable
  final case class NelPV(nel: NonEmptyList[String]) extends PathValue
  final case class ListPV(l: List[String]) extends PathValue
  final case class OptionalPV(o: Option[String]) extends PathValue
  final case class PV(s: String) extends PathValue

  private[dynamapath] def segments(s: String): List[String] =
    s.split('/').drop(1).toList 

  def fromPath(s: String, path: Path): Option[Map[String, PathValue]] =
    fromPathSegments(segments(s), path)

  def fromPathSegments(segments: List[String], path: Path): Option[Map[String, PathValue]] ={
    val matchesPathReqs: Boolean = path.path
      .zipWithIndex
      .forall {
        case (Constant(value), index) =>
          if (segments.isDefinedAt(index)) segments(index) === value
          else false
        case (Variable(_), index) =>
          if (segments.isDefinedAt(index)) true
          else false
        case (OneOrMoreVariable(_), index) =>
          if (segments.isDefinedAt(index)) true
          else false
        case (ZeroOrMoreVariable(_), _) =>
          true
        case (OptVariable(_), _) =>
          true
      }

    // Alignment
    // Bad Grammars will result in shortened but matching results.
    @tailrec
    def go(list: List[String], path: List[PathSegment], acc: Map[String, PathValue]): Option[Map[String, PathValue]] = {
      (list, path) match {
        case (Nil, OptVariable(s) :: xs) => go(Nil, xs, acc + (s -> OptionalPV(None)))
        case (Nil, _) => acc.some
        case (_ :: _, Nil) => None
        case (x :: xs,  OneOrMoreVariable(s) :: _) => (acc + (s -> NelPV(NonEmptyList(x, xs)))).some
        case (rest,  ZeroOrMoreVariable(s):: _) => (acc + (s -> ListPV(rest))).some
        case (_ :: xs, Constant(_) :: ys) => go(xs, ys, acc)
        case (x :: xs, OptVariable(s):: ys) => go(xs, ys, acc + (s -> OptionalPV(x.some)))
        case (x :: xs, Variable(s) :: ys) => go(xs, ys, acc + (s -> PV(x)))
      }
    }

    for {
      _ <- Alternative[Option].guard(matchesPathReqs)
      out <- go(segments, path.path, Map.empty)
    } yield out
  }

  def renderPathSegments(path: Path, variables: Map[String, PathValue]): Either[NonEmptyList[String], List[String]] = {
    path.path.parFlatTraverse{
      case Constant(value) => Either.right(List(value))
      case Variable(name) => variables.get(name).toRight(NonEmptyList.of(s"Missing Variable $name"))
        .flatMap{
          case PV(s) => Either.right(List(s))
          case other => Either.left(NonEmptyList.of(s"Variable: $name, not of type PV got - $other"))
        }
      case OptVariable(name) => variables.get(name)
        .fold(Either.right[NonEmptyList[String], List[String]](List.empty)){
          case PV(s) =>  Either.right(List(s))
          case OptionalPV(o) => o match {
            case None => Either.right(List.empty)
            case Some(s) => Either.right(List(s))
          }
          case other => Either.left(NonEmptyList.of(s"Variable: $name, not of type PV or OptionalPV got - $other"))
        }
      case OneOrMoreVariable(name) => variables.get(name).toRight(NonEmptyList.of(s"Missing OneOrMoreVariable $name"))
        .flatMap{
          case PV(s) => Either.right(List(s))
          case NelPV(nel) => Either.right(nel.toList)
          case other => Either.left(NonEmptyList.of(s"Variable: $name, not of type PV or NelPV got - $other"))
        }
      case ZeroOrMoreVariable(name) => variables.get(name)
        .fold(Either.right[NonEmptyList[String], List[String]](List.empty)){
          case PV(s) =>  Either.right(List(s))
          case OptionalPV(o) => o match {
            case None => Either.right(List.empty)
            case Some(s) => Either.right(List(s))
          }
          case NelPV(nel) => Either.right(nel.toList)
          case ListPV(l) => Either.right(l)
        }
    }
  }

  def renderPath(path: Path, variables: Map[String, PathValue]): Either[NonEmptyList[String], String] =
    renderPathSegments(path, variables).map(s => 
      "/" + s.intercalate("/")
    ) 


}