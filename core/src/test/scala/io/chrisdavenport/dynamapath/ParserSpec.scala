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

  "Path Renderer" should {
    "render an optional variable with no present variable" in {
      val init = "/bar"
      val p = Path(List(
        Constant("bar"),
        OptVariable("colors")
      ))

      Parser.renderPath(p, Map.empty) must beRight.like {
        case s => s must_=== init
      }
    }

    "render a present optional variable" in {
      val init = "/bar/orange"
      val p = Path(List(
        Constant("bar"),
        OptVariable("colors")
      ))
      val variables = Map(
        "colors" -> OptionalPV(Some("orange"))
      )
      Parser.renderPath(p, variables) must beRight.like {
        case s => s must_=== init
      }
    }

    "render a path optional variable with a variable" in {
      val init = "/bar/orange"
      val p = Path(List(
        Constant("bar"),
        OptVariable("colors")
      ))
      val variables = Map(
        "colors" -> PV("orange")
      )
      Parser.renderPath(p, variables) must beRight.like {
        case s => s must_=== init
      }
    }

    "render a path variable when present" in {
      val init = "/bar/orange"
      val p = Path(List(
        Constant("bar"),
        Variable("colors")
      ))
      val variables = Map(
        "colors" -> PV("orange")
      )
      Parser.renderPath(p, variables) must beRight.like {
        case s => s must_=== init
      }
    }

    "fail to render if a path variable is not present" in {
      val p = Path(List(
        Constant("bar"),
        Variable("colors")
      ))
      val variables : Map[String, PathValue] = Map.empty
      Parser.renderPath(p, variables) must beLeft.like {
        case s => s must_=== NonEmptyList.of(
          "Missing Variable colors"
        )
      }
    }

    "render a  path nonempty variable with a nonempty variable present" in {
      val init = "/bar/orange/yellow"
      val p = Path(List(
        Constant("bar"),
        OneOrMoreVariable("colors")
      ))
      val variables = Map(
        "colors" -> NelPV(NonEmptyList.of("orange", "yellow"))
      )
      Parser.renderPath(p, variables) must beRight.like {
        case s => s must_=== init
      }
    }

    "render a path nonempty variable with a variable present" in {
      val init = "/bar/orange"
      val p = Path(List(
        Constant("bar"),
        OneOrMoreVariable("colors")
      ))
      val variables = Map(
        "colors" -> PV("orange")
      )
      Parser.renderPath(p, variables) must beRight.like {
        case s => s must_=== init
      }
    }

    "fail to render a path nonempty variable when not present" in {
      val p = Path(List(
        Constant("bar"),
        OneOrMoreVariable("colors")
      ))
      val variables : Map[String, PathValue] = Map.empty
      Parser.renderPath(p, variables) must beLeft.like {
        case s => s must_=== NonEmptyList.of(
          "Missing OneOrMoreVariable colors"
        )
      }
    }

  }

}