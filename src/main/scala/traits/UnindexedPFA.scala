// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.traits
import org.maraist.fa.styles.ProbabilisticAutomatonStyle

/** Trait of the basic usage operations on any PFA.
 *
 *  @tparam S The type of all states of the automaton.
 *  @tparam T The type of labels on transitions of the automaton.
  * @tparam Z Type of style options for Graphviz export.
 *
 * @group PFA
 */
trait UnindexedPFA[S, T, -Z[S, T] <: ProbabilisticAutomatonStyle[S, T]]

extends UnindexedFA[S, T, Z] {

  /** Returns the probability that `s` is an initial state of this
    * automaton.
   */
  def initialStateProb(s: S): Double

  /** Returns the probability that `s` is a final state of this
    * automaton.
    */
  def finalStateProb(s: S): Double

  /** Returns `true` if `s` has a non-zero chance of being a final state
   *  of the automaton
   */
  def isFinalState(s: S): Boolean
  // TODO in full // = finalStateProb(s)>0.0

  // /** {@inheritDoc} For PFAs, this method returns the states which map
  //  *  via {@link org.maraist.fa.PFA#finalStateProb} to a non-zero
  //  *  probability.
  //  */
  // override def finalStates: Set[S] = {
  //   val result = new HashSet[S]
  //   for(m <- states) { if (finalStateProb(m)>0.0) { result += m } }
  //   result.toSet
  // }
  //
  // /** Returns `true` if `s` has a non-zero chance of being an initial
  //  *  state of the automaton */
  // override def isInitialState(s: S): Boolean = initialStateProb(s) > 0.0
  //
  // /** For PFAs, this method returns the states which map via {@link
  //  *  org.maraist.fa.PFA#initialStateProb} to a non-zero probability.
  //  */
  // override def initialStates: Set[S] = {
  //   val result = Set.newBuilder[S]
  //   for(m <- states) { if (initialStateProb(m)>0.0) { result += m } }
  //   result.result()
  // }

  /** Calculate the probability that the automaton accepts the given sequence
   */
  def acceptsProb(ts: Seq[T]): Double

  /** Returns the states into which the automaton could transition with
   * non-zero probability starting from `s` via a transition labelled `t`.
   *
   * In the resulting map, each resulting state is associated with the
   * probability of that transition taking place.
   */
  def possibleTransitions(s: S, t: T): Map[S, Double]

  /** Returns the probability that from state s0 and with label t, this state
   * would transition into state s1.
   */
  def transitionProb(s0: S, t: T, s1: S): Double

  /** Returns the probability of a transition state s0 into state s1 without
   *  a labeled event.
   */
  def eTransitionProb(s0:S, s1:S): Double

  // override def toString():String = {
  //   val bld:StringBuilder = new StringBuilder
  //   for (st <- this.states) {
  //     if (isInitialState(st)) bld ++= "> " else bld ++= "  "
  //     bld ++= st.toString() + "\n"
  // //       for (tr <- labels)
  // //         transition(st, tr) match {
  // //           case Some(x) => bld ++= ("  - " + tr + " --> " + x + "\n")
  // //           case None =>
  // //         }
  //   }
  //   bld.toString()
  // }

  protected[fa] def getEdgeProbTotals: Map[S, Map[T, Double]]
  // = {
  //   val totals = new HashMap[S, HashMap[T,Double]]
  //
  //   traverse(new PFAtraverser[S, T, PFA[S,T]] {
  //     override def state(
  //       pfa: PFA[S,T], i:Int, s:S, init:Double, fin:Double
  //     ) = {
  //       totals += ((s, new HashMap[T,Double]()))
  //     }
  //     override def presentEdge(pfa: PFA[S,T], fromIndex:Int, fromState:S,
  //                              labelIndex:Int, label:T,
  //                              toIndex:Int, toState:S, prob:Double) = {
  //       val smap = totals(fromState)
  //       if (!smap.contains(label)) { smap(label) = 0.0 }
  //       smap(label) += prob
  //     }
  //   })
  //
  //   totals.map({ case (k, vm) => (k, vm.toMap) }).toMap
  // }
}
