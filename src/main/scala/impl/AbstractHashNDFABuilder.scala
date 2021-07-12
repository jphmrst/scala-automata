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
import scala.collection.mutable.{HashMap,HashSet}
import org.maraist.fa.impl.
  {HashSetStateBuilderMixin,
    StateBuilderElement,
    InitialStateSetTrait,
    InitialStateSetTraitElements,
    FinalStateSetBuilderElement,
    HashFinalStateSetBuilderMixin,
    HashMapNondeterministicLabelledTransitionMixin,
    NondeterministicLabelledTransitionMixinElements,
    UnlabelledTransitionBuilderElements}
import org.maraist.fa.{NDFA, NDFABuilder}
import org.maraist.fa.NDFA.NDFAelements
import org.maraist.fa.DFA.IndexedDFA
import org.maraist.fa.elements.*

/** Implementation of [[org.maraist.fa.NDFABuilder NDFABuilder]] using
  * [[scala.collection.mutable.HashMap `HashMap`s]] and
  * [[scala.collection.mutable.HashSet `HashSet`s]]
  * @constructor Returns a builder holding an initially empty automaton
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  *
  * @group NDFA
  */
abstract class AbstractHashNDFABuilder
  [S, T, +ThisDFA <: IndexedDFA[Set[S],T],
    +ThisNDFA <: NDFA[S,T,ThisDFA],
    K >: NDFAelements[S,T] <: Matchable
  ]
    extends NDFABuilder[S, T, ThisDFA, ThisNDFA, K]
    with HashSetStateBuilderMixin[S, T]
    with HashFinalStateSetBuilderMixin[S, T]
    with InitialStateSetTrait[S, T]
    with HashMapNondeterministicLabelledTransitionMixin[S, T]
    with HashMapUnlabelledTransitionMixin[S] {

  private[fa] def deleteTransitionsFrom(s:S) = {
    transitionsMap -= s
    for(lmap <- transitionsMap.valuesIterator)
      for(v <- for(v <- lmap.keysIterator if (lmap(v).equals(s))) yield v)
        lmap -= v
    epsilons -= s
    for(v <- epsilons.valuesIterator) v -= s
  }

  /** @deprecated */
  def toNDFA: ThisNDFA = result()

  /** Creates an immutable [[org.maraist.fa.ArrayNDFA ArrayNDFA]]
    * corresponding to the automaton described to this builder.
    */
  def result(): ThisNDFA = {
    val statesSeq: IndexedSeq[S] = IndexedSeq.from(allStates)
    val transitionsSeq: IndexedSeq[T] = IndexedSeq.from(labels)
    val initials: HashSet[Int] = new HashSet[Int]
    for(s <- getInitialStates) initials += statesSeq.indexOf(s)
    val finals: HashSet[Int] = new HashSet[Int]
    for(s <- finalStatesSet) finals += statesSeq.indexOf(s)
    val empty = new HashSet[Int]

    val labelsArray = Array.ofDim[HashSet[Int]](statesSeq.size,
                                                transitionsSeq.size)
    val epsilonsArray = Array.ofDim[HashSet[Int]](statesSeq.size)
    for(si:Int <- 0 until statesSeq.length) {
      val s:S = statesSeq(si)
      for(ti:Int <- 0 until transitionsSeq.length) {
        val t:T = transitionsSeq(ti)
        labelsArray(si)(ti) =
          transitionsMap.get(s) match {
            case Some(curry) =>
              curry.get(t).fold(empty)(_.map(statesSeq.indexOf(_)))
            case None => empty
          }
        }

      epsilonsArray(si) =
        epsilons.get(s).fold(empty)(_.map(statesSeq.indexOf(_)))
    }

    // println(epsilonsArray)
    assembleNDFA(
      statesSeq, initials.toSet, finals.toSet, transitionsSeq, labelsArray,
      epsilonsArray)
  }

  protected def assembleNDFA(
    statesSeq: IndexedSeq[S],
    initials: Set[Int],
    finals: Set[Int],
    transitionsSeq: IndexedSeq[T],
    labelsArray: Array[Array[HashSet[Int]]],
    epsilonsArray: Array[HashSet[Int]]):
      ThisNDFA

  /** Helper method for the [[scala.collection.mutable.Builder]]
    * implementation.
    */
  protected def addBuilderElement(builder: K): Unit =
    builder match {
      case e: InitialStateSetTraitElements[S, T] =>
        dispatchInitialStateSetTraitElements(e)
      case e: StateBuilderElement[S, T] =>
        dispatchStateBuilderElement(e)
      case e: FinalStateSetBuilderElement[S, T] =>
        dispatchFinalStateSetHashBuilderElement(e)
      case e: UnlabelledTransitionBuilderElements[S] =>
        dispatchUnlabelledTransitionBuilderElements(e)
      case e: NondeterministicLabelledTransitionMixinElements[S, T] =>
        dispatchNondeterministicLabelledTransitionMixinElements(e)
    }
}
