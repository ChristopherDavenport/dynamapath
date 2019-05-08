package io.chrisdavenport.dynamapath

import Parser._
import org.specs2.mutable.Specification
import atto._
import Atto._
import _root_.cats.data.NonEmptyList

class ParserSpec extends Specification {

  "PathSegmentParser" should {
    "convert a constant" in {
      val a = "a"
      Parser.PATH_SEGMENT.parseOnly(a).option must_=== Some(Constant(a))
    }

    "convert a variable" in {
      val i = ":iid"
      Parser.PATH_SEGMENT.parseOnly(i).option must_=== Some(Variable("iid"))
    }

    "convert an optional variable" in {
      val i = ":opt0?"
      Parser.PATH_SEGMENT.parseOnly(i).option must_=== Some(OptVariable("opt0"))
    }

    "convert a zero or more variable" in {
      val i = ":foo*"
      Parser.PATH_SEGMENT.parseOnly(i).option must_=== Some(ZeroOrMoreVariable("foo"))
    }

    "convert a one or more variable" in {
      val i = ":foo+"
      Parser.PATH_SEGMENT.parseOnly(i).option must_=== Some(OneOrMoreVariable("foo"))
    }
  }

  "PathParser" should {
    "convert a known path in " in {
      val init = "/foo/bar/baz/node/banana/:iid/:type/:opt0?/:opt1?"

      val initOut = Path(List(
        Constant("foo"),
        Constant("bar"),
        Constant("baz"),
        Constant("node"),
        Constant("banana"),
        Variable("iid"),
        Variable("type"),
        OptVariable("opt0"),
        OptVariable("opt1")
      ))

      Parser.PATH.parseOnly(init).option must_=== Some(initOut)
    }
  }

  "Path router" should {
    "Generate Some for a Matching List of Constants" in {
      val init = "/foo/bar/baz/node/banana/foo/green/some1/some2"
      val initOut = Path(List(
        Constant("foo"),
        Constant("bar"),
        Constant("baz"),
        Constant("node"),
        Constant("banana"),
        Variable("iid"),
        Variable("type"),
        OptVariable("opt0"),
        OptVariable("opt1")
        ))

      Parser.fromPath(init, initOut) must beSome
    }

    "Generate A Known Map Matching List of Constants" in {
      val init = "/foo/bar/baz/node/banana/foo/green/some1/some2"
      val initOut = Path(List(
        Constant("foo"),
        Constant("bar"),
        Constant("baz"),
        Constant("node"),
        Constant("banana"),
        Variable("iid"),
        Variable("type"),
        OptVariable("opt0"),
        OptVariable("opt1")
        ))

      Parser.fromPath(init, initOut) must_=== Some(
        Map(
          "iid" -> PV("foo"),
          "type" -> PV("green"),
          "opt0" -> OptionalPV(Some("some1")),
          "opt1" -> OptionalPV(Some("some2"))
        )
      )
    }

    "Generate A Known Map Matching List of Constants with Missing Optionals" in {
      val init = "/foo/bar/baz/node/banana/foo/green"
      val initOut = Path(List(
          Constant("foo"),
          Constant("bar"),
          Constant("baz"),
          Constant("node"),
          Constant("banana"),
          Variable("iid"),
          Variable("type"),
          OptVariable("opt0"),
          OptVariable("opt1")
        ))

      Parser.fromPath(init, initOut) must_=== Some(
        Map(
          "iid" -> PV("foo"),
          "type" -> PV("green"),
          "opt0" -> OptionalPV(None),
          "opt1" -> OptionalPV(None)
        )
      )
    }

    "Work with a finishing non-empty list" in {
      val init = "/bar/yellow/red/green"
      val p = Path(List(
        Constant("bar"),
        OneOrMoreVariable("colors")
      ))
      Parser.fromPath(init, p) must_=== Some(
        Map(
          "colors" -> NelPV(NonEmptyList.of("yellow", "red", "green"))
        )
      )
    }

    "Not Match A Missing Ending Variable" in {
      val init = "/bar"
      val p = Path(List(
        Constant("bar"),
        Variable("colors")
      ))
      Parser.fromPath(init, p) must_=== None
    }

    "Not Match A Missing Ending OneOrMoreVariable" in {
      val init = "/bar"
      val p = Path(List(
        Constant("bar"),
        OneOrMoreVariable("colors")
      ))
      Parser.fromPath(init, p) must_=== None
    }

  }

}