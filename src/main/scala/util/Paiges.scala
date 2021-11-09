// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.util
import org.typelevel.paiges.Doc

trait Pretty:
    def pretty: Doc
end Pretty

object Paiges {

  extension (obj: Matchable) {
    def toDoc: Doc = obj match {
      case pr: Pretty => pr.pretty
      case doc: Doc => doc
      case s: String => Doc.text(s)
      case opt: Option[? <: Matchable] => opt
          .map(_.toDoc.tightBracketBy(Doc.text("Some("), Doc.text(")")))
          .getOrElse(Doc.text("None"))

      // This cannot be the best way to do this.
      case c: Set[? <: Matchable] => collectionToDoc("Set", c)
      case c: List[? <: Matchable] => collectionToDoc("List", c)
      case c: Array[? <: Matchable] => collectionToDoc("Array", c)
      case c: Seq[? <: Matchable] => collectionToDoc("Seq", c)
      case c: Iterable[? <: Matchable] => collectionToDoc("Iterable", c)

      case _ => Doc.str(obj)
    }
  }

  private def collectionToDoc[X <: Matchable, C[Y] <: Iterable[Y]](
    name: String, coll: C[X]):
      Doc =
    ((coll.map(_.toDoc).fill(Doc.text(",") + Doc.space) + Doc.text(")"))
      .tightPrefixBy(Doc.text(s"$name(")))
      .grouped

  extension (doc: Doc) {
    def prefixBy(left: Doc, indent: Int = 2): Doc =
      (left + (Doc.line + doc).nested(indent)).grouped
    def tightPrefixBy(left: Doc, indent: Int = 2): Doc =
      (left + (Doc.lineOrEmpty + doc).nested(indent)).grouped
    def suffixBy(right: Doc, indent: Int = 2): Doc =
      (doc.nested(indent) + (Doc.line + right)).grouped
    def tightSuffixBy(right: Doc, indent: Int = 2): Doc =
      (doc.nested(indent) + (Doc.lineOrEmpty + right)).grouped
  }

  extension (docs: Iterable[Doc]) {
    /**
      * Collapse a collection of documents into one document,
      * delimited by a separator.  Simply calls the `Doc.fill` method.
      *
      * For example:
      *
      *     import Doc.{ comma, line, text, fill }
      *     val ds = text("1") :: text("2") :: text("3") :: Nil
      *     val doc = ds.fill(comma + line)
      *
      *     doc.render(0)  // produces "1,\n2,\n3"
      *     doc.render(6)  // produces "1, 2,\n3"
      *     doc.render(10) // produces "1, 2, 3"
      */
    def fill(s: Doc): Doc = Doc.fill(s, docs)
    /**
      * Concatenate the given documents together, delimited by newlines.
      */
    def stack: Doc = Doc.stack(docs)
  }
}

