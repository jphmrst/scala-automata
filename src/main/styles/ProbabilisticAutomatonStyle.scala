// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.styles
import org.maraist.graphviz.{Graphable, GraphStyle}
import org.maraist.fa.traits.UnindexedEdgeAnnotatedFA

open class ProbabilisticAutomatonStyle[S, T](
  // An identifier for this AutomatonStyle
  id: String = "",

  // Process-related
  format: String = "pdf",
  srcSuffix: String = "dot",
  executable: String = "dot",

  // Graph properties
  keepDOT: Boolean = false,
  fontSize: Int = GraphStyle.defaultFontSize,
  margin: Double = GraphStyle.defaultMargin,

  // Node properties
  nodeShape: (S, Graphable[S, T, ?]) => String =
    (s: S, _: Graphable[S, T, ?]) => "circle",
  nodeLabel: (S, Graphable[S, T, ?]) => String =
    (s: S, _: Graphable[S, T, ?]) => s.toString(),
  finalNodeShape: (S, Graphable[S, T, ?]) => String =
    (s: S, _: Graphable[S, T, ?]) => "doublecircle",

  // Edge properties
  edgeLabel: (T, S, S, Graphable[S, T, ?]) => String =
    (t: T, _: S, _: S, _: Graphable[S, T, ?]) => t.toString()
)

extends AutomatonStyle[S, T](
  id, format, srcSuffix, executable, keepDOT, fontSize, margin,
  nodeShape, nodeLabel, finalNodeShape,
  edgeLabel) {

}

object ProbabilisticAutomatonStyle {
  given implicitProbabilisticAutomatonStyle[S, T]:
      ProbabilisticAutomatonStyle[S, T] =
    new ProbabilisticAutomatonStyle[S, T](
      "edge-annotated-automata-style-default")

  def derivedFrom[
    S, T,
    Z[ZS, ZT] <: ProbabilisticAutomatonStyle[ZS, ZT]
  ](using base: Z[S, T]
  )(
    id: String = "",
    format: String = base.format,
    srcSuffix: String = base.srcSuffix,
    executable: String = base.executable,
    keepDOT: Boolean = base.keepDOT,
    fontSize: Int = base.fontSize,
    margin: Double = base.margin,
    nodeShape: (S, Graphable[S, T, ?]) => String = base.nodeShape,
    finalNodeShape: (S, Graphable[S, T, ?]) => String =
      base.finalNodeShape,
    nodeLabel: (S, Graphable[S, T, ?]) => String = base.nodeLabel,
    edgeLabel: (T, S, S, Graphable[S, T, ?]) => String = base.edgeLabel
  ) =
    new ProbabilisticAutomatonStyle[S, T](
      id, format, srcSuffix, executable, keepDOT, fontSize, margin, nodeShape,
      finalNodeShape, nodeLabel, edgeLabel)
}
