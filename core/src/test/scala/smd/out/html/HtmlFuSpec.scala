package smd
package out
package html

import org.scalatest.{FunSpec, Matchers}

class HtmlFuSpec extends FunSpec with Matchers {
  import smd.util.PostfixOptionImplicits._
  import HtmlFu._

  ('a.inline()()) should be (HtmlNode.Inline("a", Seq(), Seq()))
  ("a".inline()()) should be (HtmlNode.Inline("a", Seq(), Seq()))
  ('a.inline()("text")) should be (HtmlNode.Inline("a", Seq(), Seq(HtmlNode.Text(HtmlString.raw("text")))))
  ('a.inline()("<br>")) should be (HtmlNode.Inline("a", Seq(), Seq(HtmlNode.Text(HtmlString.raw("&lt;br&gt;")))))
  'a.inline()('strong.inline()("text")) should be (HtmlNode.Inline("a", Seq(), Seq(HtmlNode.Inline("strong", Seq(), Seq(HtmlNode.Text(HtmlString.raw("text")))))))

  'a.inline('foo -> "bar" when true)() should be (HtmlNode.Inline("a", Seq(HtmlAttribute.Value("foo", "bar")), Seq()))
  'a.inline('foo -> "bar" when false)() should be (HtmlNode.Inline("a", Seq(), Seq()))
  'input.empty('hidden when true) should be (HtmlNode.Empty("input", Seq(HtmlAttribute.Boolean("hidden"))))
  'input.empty('hidden when false) should be (HtmlNode.Empty("input", Seq()))

  'p.block('class -> "para")(
    "This is some ", 'strong.inline()("strong"), " text."
  ) should be (
    HtmlNode.Block("p", Seq(HtmlAttribute.Value("class", "para")), Seq(
      HtmlNode.Text("This is some "),
      HtmlNode.Inline("strong", Seq(), Seq(
        HtmlNode.Text("strong")
      )),
      HtmlNode.Text(" text.")
    ))
  )
}
