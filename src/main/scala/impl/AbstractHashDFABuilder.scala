// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.impl
import scala.collection.mutable.{Builder,Growable,HashMap,HashSet}
import org.maraist.fa.{DFA, DFABuilder}
import org.maraist.fa.impl.
  {SingleInitialStateMixin, HashSetStateBuilderMixin,
    SingleInitialStateMixinElement, HashFinalStateSetBuilderMixin,
    StateBuilderElement, FinalStateSetBuilderElement,
    DeterministicLabelledTransitionMixin,
    DeterministicLabelledTransitionMixinElement}
import org.maraist.fa.elements.*
import org.maraist.fa.DFA.*

/** Implementation of [[org.maraist.fa.DFABuilder DFABuilder]] using
 *  [[scala.collection.mutable.HashSet `HashSet`s]] and
 *  [[scala.collection.mutable.HashMap `HashMap`s]].
 * @param initialState The initial state of the automaton.  This builder must
 * be instantiated with one state, initially taken to be the initial state.
 * @tparam S The type of all states of the automaton
 * @tparam T The type of labels on (non-epsilon) transitions of the automaton
 *
 * @group DFA
 */
abstract class AbstractHashDFABuilder[
  S, T,
  ThisDFA <: AbstractArrayDFA[S,T],
  K >: DFAelements[S,T] <: Matchable
](initialState: S)
    extends SingleInitialStateMixin[S,T](initialState)
    with DFABuilder[S,T, ThisDFA, K]
    with HashSetStateBuilderMixin[S,T]
    with HashFinalStateSetBuilderMixin[S,T]
    with DeterministicLabelledTransitionMixin[S, T] {

  import scala.util.control.NonLocalReturns.*
  def accepts(ts:Seq[T]): Boolean = returning {
    var current:S = getInitialState
    for(t <- ts) {
      transition(current, t) match {
        case Some(s) => { current = s }
        case None => throwReturn(false)
      }
    }
    isFinalState(current)
  }

  /** @deprecated */
  def toDFA: ThisDFA = result()

  def result(): ThisDFA = {
    val statesSeq: IndexedSeq[S] = IndexedSeq.from(allStates)
    val transitionsSeq: IndexedSeq[T] = IndexedSeq.from(labels)
    val initialIdx: Int = statesSeq.indexOf(getInitialState)
    val finalStateIndices: HashSet[Int] = new HashSet[Int]
    for(s <- finalStatesSet) finalStateIndices += statesSeq.indexOf(s)

    val idxLabels:Array[Array[Int]] =
      Array.ofDim[Int](statesSeq.length, transitionsSeq.length)
    for(si <- 0 until statesSeq.length) {
      val s:S = statesSeq(si)
      for(ti <- 0 until transitionsSeq.length) {
        val t:T = transitionsSeq(ti)
        if (transitionsMap.contains(s)) {
          if (transitionsMap(s).contains(t)) {
            idxLabels(si)(ti) = statesSeq.indexOf(transitionsMap(s)(t))
          } else {
            idxLabels(si)(ti) = -1
          }
        } else {
          idxLabels(si)(ti) = -1
        }
      }
    }

    assembleDFA(statesSeq, initialIdx, finalStateIndices, transitionsSeq,
                idxLabels)
  }

  protected def assembleDFA(statesSeq: IndexedSeq[S], initialIdx: Int,
                            finalStateIndices: HashSet[Int],
                            transitionsSeq: IndexedSeq[T],
                            idxLabels: Array[Array[Int]]): ThisDFA

  /** Helper method for the [[scala.collection.mutable.Builder]]
    * implementation.
    */
  protected def addBuilderElement(builder: K): Unit =
    builder match {
      case e: SingleInitialStateMixinElement[S, T] =>
        dispatchSingleInitialStateMixinElement(e)
      case e: StateBuilderElement[S, T] =>
        dispatchStateBuilderElement(e)
      case e: FinalStateSetBuilderElement[S, T] =>
        dispatchFinalStateSetHashBuilderElement(e)
      case e: DeterministicLabelledTransitionMixinElement[S, T] =>
        dispatchDeterministicLabelledTransitionMixinElement(e)
    }
}
