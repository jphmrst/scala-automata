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

object Paiges {
  extension (doc: Doc) {
    def prefixBy(left: Doc, indent: Int = 2): Doc =
      (left + (Doc.line + doc).nested(indent)).grouped
    def suffixBy(right: Doc, indent: Int = 2): Doc =
      (doc.nested(indent) + (Doc.line + right)).grouped
  }

  extension (docs: Iterable[Doc]) {
    /**
      * Collapse a collection of documents into one document,
      * delimited by a separator.  Simply calls the [[Doc#fill]]
      * method.
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

