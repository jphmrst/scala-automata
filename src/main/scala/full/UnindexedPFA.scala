// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.full
import scala.collection.mutable.HashMap
import org.maraist.fa.styles.{ProbabilisticAutomatonStyle, DOT}
import org.maraist.fa.traits

/** Trait of the basic usage operations on any PFA.
 *
 *  @tparam S The type of all states of the automaton.
 *  @tparam T The type of labels on transitions of the automaton.
  * @tparam Z Type of style options for Graphviz export.
 *
 * @group PFA
 */
trait UnindexedPFA[S, T, -Z[S, T] <: ProbabilisticAutomatonStyle[S, T]]

extends traits.UnindexedPFA[S, T, Z]

with UnindexedFA[S, T, Z] {

  override def isFinalState(s: S): Boolean = finalStateProb(s)>0.0

  /** {@inheritDoc} For PFAs, this method returns the states which map
    *  via {@link org.maraist.fa.PFA#finalStateProb} to a non-zero
    *  probability.
    */
  override def finalStates: Set[S] = {
    val result = Set.newBuilder[S]
    for(m <- states) { if (finalStateProb(m)>0.0) { result += m } }
    result.result
  }

  /** {@inheritDoc} For PFAs, this method returns `true` if `s` has a
    *  non-zero chance of being an initial state of the automaton.
    */
  override def isInitialState(s: S): Boolean = initialStateProb(s) > 0.0

  /** {@inheritDoc} For PFAs, this method returns the states which map
    *  via {@link org.maraist.fa.PFA#initialStateProb} to a non-zero
    *  probability.
    */
  override def initialStates: Set[S] = {
    val result = Set.newBuilder[S]
    for(m <- states) { if (initialStateProb(m)>0.0) { result += m } }
    result.result()
  }

  def possibleTransitions(s: S, t: T): Map[S, Double] = {
    val builder = Map.newBuilder[S, Double]
    foreachTransition((s1: S, t: T, s2: S, prob: Double) => {
      if (prob > 0.0) then builder += ((s2, prob))
    })
    builder.result
  }

  override def toString():String = {
    val bld: StringBuilder = new StringBuilder
    for (st <- this.states) {
      if (isInitialState(st)) bld ++= "> " else bld ++= "  "
      bld ++= st.toString() + "\n"
      //       for (tr <- labels)
      //         transition(st, tr) match {
      //           case Some(x) => bld ++= ("  - " + tr + " --> " + x + "\n")
      //           case None =>
      //         }
    }
    bld.toString()
  }

  protected[fa] def getEdgeProbTotals: Map[S, Map[T, Double]] = {
    val totals = new HashMap[S, HashMap[T, Double]]
    foreachState((s: S) => { totals += ((s, new HashMap[T, Double]())); {} })
    foreachTransition((s1: S, t: T, s2: S, prob: Double) => {
      val smap = totals(s1)
      if (!smap.contains(t)) { smap(t) = 0.0 }
      smap(t) += prob
    })
    totals.map({
      case (k, vm) => (k, vm.toMap)
    }).toMap
  }

  // =================================================================

  override protected def plotInitialStateMarker(
    sb: StringBuilder, s: S, idx: Int):
      Unit =
    plotInitialStateMarker(sb, s, idx, initialStateProb(s))

  protected def plotInitialStateMarker(
    sb: StringBuilder, s: S, si: Int, prob: Double):
      Unit = {
    sb ++= "\tI"
    sb ++= si.toString()
    sb ++= " [shape=none, margin=0, label=\"\"]\n\tI"
    sb ++= si.toString()
    sb ++= DOT.graphvizArrowToVmark
    sb ++= si.toString()
    sb ++= " [ label=<<font color=\"blue\">"
    sb ++= prob.toString()
    sb ++= DOT.endFont
    sb ++= DOT.endLabelAndDot
  }

  override protected def edgeText(
    style: Z[S, T], si0: Int, s0: S, ti0: Int, t: T, si1: Int, s1: S
  ): String =
    (style.edgeLabel(t, s0, s1, this)
      ++ "; <font color=\"blue\">"
      ++ transitionProb(s0, t, s1).toString()
      ++ "</font>")

  override protected def edgeText(
    style: Z[S, T], si0: Int, s0: S, si1: Int, s1: S
  ): String = {
    val prob = eTransitionProb(s0, s1)
    if (prob > 0.0)
      then ("<font color=\"blue\">"
        ++ prob.toString()
        ++ "</font>")
    else ""
  }

}
