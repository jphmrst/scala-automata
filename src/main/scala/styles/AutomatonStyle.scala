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

open class AutomatonStyle[S, T](
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
  var finalNodeShape: (S, Graphable[S, T, ?]) => String =
    (s: S, _: Graphable[S, T, ?]) => "doublecircle",

  // Edge properties
  edgeLabel: (T, S, S, Graphable[S, T, ?]) => String =
    (t: T, _: S, _: S, _: Graphable[S, T, ?]) => t.toString(),
  var eEdgeLabel: (S, S, Graphable[S, T, ?]) => String =
    (_: S, _: S, _: Graphable[S, T, ?]) => "")

extends GraphStyle[S, T](id,
  format, srcSuffix, executable,
  keepDOT, fontSize, margin,
  nodeShape, nodeLabel,
  edgeLabel) {

  override def toString(): String = internalId

  def this(opts: AutomatonStyle[S, T]) = {
    this(
      opts.internalId,
      opts.format, opts.srcSuffix, opts.executable,
      opts.keepDOT, opts.fontSize, opts.margin,
      opts.nodeShape, opts.nodeLabel, opts.finalNodeShape,
      opts.edgeLabel)
  }
}

object AutomatonStyle {
  given implicitAutomatonStyle[S, T]: AutomatonStyle[S, T] =
    new AutomatonStyle[S, T]("automata-style-default")

  def derivedFrom[S, T, Style[A, B] <: AutomatonStyle[A, B]](
    using base: Style[S, T]
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
    edgeLabel: (T, S, S, Graphable[S, T, ?]) => String = base.edgeLabel,
    eEdgeLabel: (S, S, Graphable[S, T, ?]) => String = base.eEdgeLabel
  ) =
    new AutomatonStyle[S, T](
      id, format, srcSuffix, executable, keepDOT, fontSize, margin, nodeShape,
      finalNodeShape, nodeLabel, edgeLabel, eEdgeLabel)
}
