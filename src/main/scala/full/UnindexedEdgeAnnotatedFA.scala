// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.full
import org.typelevel.paiges.Doc
import org.maraist.fa.util.EdgeAnnotationCombiner
import org.maraist.fa.styles.{EdgeAnnotatedAutomatonStyle, DOT}
import org.maraist.fa.traits

/** Methods provided by an edge-annotated nondeterministic finite
  * automata (NFA).
  *
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  * @tparam A The type of annotations on transitions
  * @tparam K Type function/contructor producing the type of
  * annotation on DFAs from the type of annotation on NFAs.
  * @tparam D Type of DFA converted from this NFA
  *
  * @group Annotated
  */
trait UnindexedEdgeAnnotatedFA[
  S, T, A,
  -Z[ZS, ZT, ZA] <: EdgeAnnotatedAutomatonStyle[ZS, ZT, ZA]
]

extends traits.UnindexedEdgeAnnotatedFA[S, T, A, Z]

with UnindexedFA[S, T, [ZS, ZT] =>> Z[ZS, ZT, A]] {

  /** {@inheritDoc} */
  override def annotated(src: S, label: T, dest: S): Boolean =
    annotation(src, label, dest).isDefined

  /** {@inheritDoc} */
  override def eAnnotated(src: S, dest: S): Boolean =
    eAnnotation(src, dest).isDefined

  /** {@inheritDoc} */
  override def foreachEdgeAnnotation(action: (S, T, S, A) => Any): Unit =
    foreachTransition(
      (src: S, label: T, dest: S) => annotation(src, label, dest) match {
        case None => { }
        case Some(ann) => action(src, label, dest, ann)
      })

  /** {@inheritDoc} */
  override def foreachEdgeAnnotation(action: (S, S, A) => Any): Unit =
    foreachETransition((src: S, dest: S) => eAnnotation(src, dest) match {
      case None => { }
      case Some(ann) => action(src, dest, ann)
    })

  // =================================================================

  override protected def plotPresentEdge(
    sb: StringBuilder,
    style: Z[S, T, A],
    stateList: IndexedSeq[S],
    stateMap: Map[S, Int],
    si0: Int,
    s0: S,
    ti0: Int,
    t: T,
    si1: Int,
    s1: S
  ): Unit = {
    sb ++= DOT.tabToVmark
    sb ++= Integer.toString(si0)
    sb ++= DOT.graphvizArrowToVmark
    sb ++= Integer.toString(si1)
    sb ++= " [ label=<"
    sb ++= style.edgeLabel(t, s0, s1, this)
    annotation(s0, t, s1) match {
      case None => { }
      case Some(a) => {
        sb ++= "<br/>"
        sb ++= style.annotationLabel(a, t, s0, s1)
      }
    }
    sb ++= "> ];\n"
  }

  override protected def plotPresentEdge(
    sb: StringBuilder, style: Z[S, T, A],
    stateList: IndexedSeq[S],
    stateMap: Map[S, Int],
    si0: Int, s0: S, si1: Int, s1: S):
      Unit = {
    sb ++= DOT.tabToVmark
    sb ++= Integer.toString(si0)
    sb ++= DOT.graphvizArrowToVmark
    sb ++= Integer.toString(si1)
    val label = style.eEdgeLabel(s0, s1, this)
    val showLabel = !label.equals("")
    val ann = eAnnotation(s0, s1)
    val showAnn = ann.isDefined
    if showLabel || showAnn then sb ++= " [ label=< "
    if showLabel then sb ++= label
    ann match {
      case None => { }
      case Some(a) => {
        if showLabel then sb ++= "<br/>"
        sb ++= style.eAnnotationLabel(a, s0, s1)
      }
    }
    if showLabel || showAnn then sb ++= " > ]"
    sb ++= ";\n"
  }

  // =================================================================

  override protected def prettyHeader: Doc =
    Doc.text("---------- UnindexedEdgeAnnotatedFA dump")

  override protected
  def prettyTransitionArrow(src: S, label: T, dest: S): Doc =
    (Doc.text("-[ ") + Doc.str(label) + Doc.text(" ]->")
      + (annotation(src, label, dest) match {
        case None => Doc.text(" (unann.)")
        case Some(a) => (Doc.text(" : ") + Doc.str(a))
      })).indent(4)

  override protected def prettyETransitionArrow(src: S, dest: S): Doc =
    (Doc.text("-->")
      + (eAnnotation(src, dest) match {
        case None => Doc.text("  (unann.)")
        case Some(a) => Doc.text("  ") + Doc.str(a)
      })).indent(4)
}
