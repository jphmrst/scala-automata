// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.pfa
import scala.collection.mutable.{HashMap,HashSet}
import org.maraist.graphviz.{Graphable, GraphStyle}
import org.maraist.fa.traits.
  {StateHolder, FinalStateSetHolder, InitialStateSetHolder}
import org.maraist.fa.impl.{SingleInitialStateMixinElement}
import org.maraist.fa.elements.HasBuilder
import org.maraist.fa.pfa.impl.PFAdotTraverser
import org.maraist.fa.pfa.Builders.PFAelements

/** Trait of the basic usage operations on a PFA.
 *
 *  @tparam S The type of all states of the automaton
 *  @tparam T The type of labels on transitions of the automaton
 *
 * @group PFA
 */
trait PFA[S,T]
    extends StateHolder[S]
    // with InitialStateSetHolder[S]
    with FinalStateSetHolder[S]
    with Graphable[S,T] {

  /** Returns the probability that `s` is an initial state of this
    * automaton.
   */
  def initialStateProb(s:S): Double

  /** Returns the probability that `s` is a final state of this
    * automaton.
    */
  def finalStateProb(s:S): Double

  /** Returns `true` if `s` has a non-zero chance of being a final state
   *  of the automaton
   */
  def isFinalState(s:S): Boolean = finalStateProb(s)>0.0

  def labels: Iterable[T]

  /** {@inheritDoc} For PFAs, this method returns the states which map
   *  via {@link org.maraist.fa.PFA#finalStateProb} to a non-zero
   *  probability.
   */
  def finalStates: Set[S] = {
    val result = new HashSet[S]
    for(m <- states) { if (finalStateProb(m)>0.0) { result += m } }
    result.toSet
  }

  /** Returns `true` if `s` has a non-zero chance of being an initial
   *  state of the automaton */
  def isInitialState(s:S): Boolean = initialStateProb(s) > 0.0

  /** For PFAs, this method returns the states which map via {@link
   *  org.maraist.fa.PFA#initialStateProb} to a non-zero probability.
   */
  def getInitialStates: Set[S] = {
    val result = Set.newBuilder[S]
    for(m <- states) { if (initialStateProb(m)>0.0) { result += m } }
    result.result()
  }

  /** Calculate the probability that the automaton accepts the given sequence
   */
  def accepts(ts:Seq[T]): Double

  /** Returns the states into which the automaton could transition with
   * non-zero probability starting from `s` via a transition labelled `t`.
   *
   * In the resulting map, each resulting state is associated with the
   * probability of that transition taking place.
   */
  def transition(s:S, t:T): Map[S,Double]

  /** Returns the probability that from state s0 and with label t, this state
   * would transition into state s1.
   */
  def transition(s0:S, t:T, s1:S): Double

  /** Returns the probability of a transition state s0 into state s1 without
   *  a labeled event.
   */
  def eTransition(s0:S, s1:S): Double

  override def toString():String = {
    val bld:StringBuilder = new StringBuilder
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

   /** Traverse the structure of this DFA, states first, then transitions */
   def traverse(trav:PFAtraverser[S,T,? >: this.type]) = {
     val stateList = IndexedSeq.from(states)
     val theLabels = IndexedSeq.from(labels)
     trav.init()
     for(si <- 0 until stateList.length) {
       val s = stateList(si)
       trav.state(this, si,s,initialStateProb(s),finalStateProb(s))
     }
     trav.postState()
     for(si0 <- 0 until stateList.length) {
       val s0 = stateList(si0)

       for(si1 <- 0 until stateList.length) {
         val s1 = stateList(si1)
         val prob = eTransition(s0,s1)
         if (prob>0.0) {
           trav.presentEdge(this, si0, s0, si1, s1, prob)
         } else {
           trav.absentEpsilonEdge(this, si0, s0, si1, s1)
         }
       }

       for(ti <- 0 until theLabels.length) {
         val t = theLabels(ti)
         var present=false
         for(si1 <- 0 until stateList.length) {
           val s1 = stateList(si1)
           val prob = transition(s0,t,s1)
           if (prob>0.0) {
             trav.presentEdge(this, si0, s0, ti, t, si1, s1, prob)
             present=true
           } else {
             trav.absentEdge(this, si0, s0, ti, t, si1, s1)
           }
         }
         if (!present) {
           trav.absentEdge(this, si0, s0, ti, t)
         }
       }
     }
     trav.finish()
   }

  /** Internal routine used by {@link #toDOT}.  Subclesses should
   *  override, but still call super.internalsToDOT, to extend the
   *  Graphviz representation of a DFA */
  protected def internalsToDOT(
    stateList:IndexedSeq[S], sb:StringBuilder
  )(using
    graphvizOptions: GraphStyle[S, T]
  ): Unit = {
    traverse(new PFAdotTraverser[S,T,PFA[S,T]](
      sb, graphvizOptions))
  }

  /** {@inheritDoc} */
  def toDOT(using graphvizOptions: GraphStyle[S, T]):String = {
    val stateList = IndexedSeq.from(states)
    val sb = new StringBuilder()
    internalsToDOT(stateList,sb)
    sb.toString()
  }

  private[fa] def getEdgeProbTotals: Map[S, Map[T, Double]] = {
    val totals = new HashMap[S, HashMap[T,Double]]

    traverse(new PFAtraverser[S, T, PFA[S,T]] {
      override def state(pfa: PFA[S,T], i:Int, s:S, init:Double, fin:Double) = {
        totals += ((s, new HashMap[T,Double]()))
      }
      override def presentEdge(pfa: PFA[S,T], fromIndex:Int, fromState:S,
                               labelIndex:Int, label:T,
                               toIndex:Int, toState:S, prob:Double) = {
        val smap = totals(fromState)
        if (!smap.contains(label)) { smap(label) = 0.0 }
        smap(label) += prob
      }
    })

    totals.map({ case (k, vm) => (k, vm.toMap) }).toMap
  }
}

/**
 * Methods for traversing the structure of a {@link org.maraist.fa.PFA PFA}.
 * Use with the {@link org.maraist.fa.PFA#traverse PFA.traverse} method. By
 * default, all methods are empty.
 *
 *  @tparam S The type of all states of the automaton
 *  @tparam T The type of labels on transitions of the automaton
 * @group graphviz
 */
private[fa] trait PFAtraverser[-S,-T, P >: PFA[S,T]] {

  /** Called at the beginning of traversal, before any other methods. */
  def init():Unit = { }

  /** Called once for each state in the {@link org.maraist.fa.DFA DFA}. */
  def state(pfa: P, index:Int, state:S,
            initialProb:Double, finalProb:Double):Unit = { }

  /**
   * Called after the last call to {@link org.maraist.fa.PFAtraverser#state
   * state}, but before any calls to
   * presentEdge
   * or absentEdge.
   */
  def postState():Unit = { }

  /** Called for each epsilon-transition between states in the DFA with
   *  non-zero pronbability. */
  def presentEdge(pfa: P, fromIndex:Int, fromState:S,
                  toIndex:Int, toState:S, prob:Double):Unit = { }

  /** Called for each state pair with zero probability of an epsilon
   * transition between them.  Note that this method will be called even if
   * there is a labeled transition from `fromState` to `toState`.*/
  def absentEpsilonEdge(pfa: P, fromIndex:Int, fromState:S,
                        toIndex:Int, toState:S):Unit = { }

  /** Called for each labeled transition between states with non-zero
   *  probability in the DFA. */
  def presentEdge(pfa: P, fromIndex:Int, fromState:S, labelIndex:Int, label:T,
                  toIndex:Int, toState:S, prob:Double):Unit = { }

  /** Called for each state/label/state triple with zero probability.
   *  Note that this method will be called even if there is an epsilon
   *  transition from `fromState` to `toState`.
   */
  def absentEdge(pfa: P, fromIndex:Int, fromState:S, labelIndex:Int, label:T,
                 toIndex:Int, toState:S):Unit = { }

  /** Called for each state/label pair for which there is no target state
   *  with nonzero pobability. */
  def absentEdge(pfa: P, fromIndex:Int, fromState:S, labelIndex:Int, label:T):
      Unit = { }

  /** Called last among the methods of this trait for any traversal. */
  def finish():Unit = { }
}

/**
  *
  * @group PFA
  */
object PFA {
  def newBuilder[S, T, SetType[_], MapType[_,_]](initialState: S)(
    using impl: HasBuilder[PFAelements, ?, PFA]
  ) = impl.build[S,T]()
}
