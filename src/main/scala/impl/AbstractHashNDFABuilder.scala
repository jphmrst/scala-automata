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
import org.maraist.fa.general.
  {InitialStateSetTrait, StateHashBuilderTrait,
    FinalStateSetHashBuilderTrait, InitialStateSetTraitElements,
    StateHashBuilderElements, FinalStateSetHashBuilderElements}
import org.maraist.fa.{NDFA, NDFABuilder}
import org.maraist.fa.NDFA.{AddETransition, RemoveETransition, NDFAelements}
import org.maraist.fa.DFA.IndexedDFA
import org.maraist.fa.general.Builders.*

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
    with StateHashBuilderTrait[S, T]
    with FinalStateSetHashBuilderTrait[S, T]
    with InitialStateSetTrait[S, T] {

  /** Maps from a state `s` and label `t` to the set of states at the
    * end of transitions starting from `s` and labeled `t` */
  val transitionsMap = new HashMap[S,HashMap[T,HashSet[S]]]
  /** Maps from a state `s` to the set of states at the end of
    * &epsilon;-transitions starting from `s` */
  val epsilons:HashMap[S,HashSet[S]] = new HashMap[S,HashSet[S]]

  private[fa] def deleteTransitionsFrom(s:S) = {
    transitionsMap -= s
    for(lmap <- transitionsMap.valuesIterator)
      for(v <- for(v <- lmap.keysIterator if (lmap(v).equals(s))) yield v)
        lmap -= v
    epsilons -= s
    for(v <- epsilons.valuesIterator) v -= s
  }

  def addTransition(s1:S, t:T, s2:S):Unit = {
    addState(s1)
    addState(s2)
    val submap:HashMap[T,HashSet[S]] =
      transitionsMap.getOrElseUpdate(s1,new HashMap[T,HashSet[S]])
    val subsubmap:HashSet[S] = submap.getOrElseUpdate(t, new HashSet[S])
    subsubmap += s2
  }
  def addETransition(s1:S, s2:S):Unit = {
    // println("** " + s1 + " --> " + s2)
    addState(s1)
    addState(s2)
    val s1Set:HashSet[S] = epsilons.getOrElseUpdate(s1, new HashSet[S])
    s1Set += s2
  }
  def removeTransition(s1:S, t:T, s2:S):Unit = {
    if (transitionsMap.contains(s1)) {
      val submap:HashMap[T,HashSet[S]] = transitionsMap(s1)
      if (submap.contains(t)) submap(t) -= s2
    }
  }
  def removeETransition(s1:S, s2:S):Unit = {
    if (epsilons.contains(s1)) epsilons(s1) -= s2
  }

  /** @inheritdoc
    *
    * This method is always calculated by traversing the `transitionsMap` map;
    * it is not cached.
    */
  def labels: Set[T] = {
    val result = new HashSet[T]()
    for(map <- transitionsMap.valuesIterator)
      for(t <- map.keysIterator)
        result += t
    result.toSet
  }

  def transitions(s:S, t:T): Set[S] = {
    if (transitionsMap.contains(s)) {
      val submap:HashMap[T,HashSet[S]] = transitionsMap(s)
      if (submap.contains(t))
        submap(t).toSet
      else
        Set.empty[S]
    } else
      Set.empty[S]
  }
  def eTransitions(s:S): Set[S] = {
    if (epsilons.contains(s))
      epsilons(s).toSet
    else
      Set.empty[S]
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
    for(s <- initialStates) initials += statesSeq.indexOf(s)
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
  protected def addBuilderElement(builder: NDFAelements[S, T]): Unit =
    builder match {
      case e: InitialStateSetTraitElements[S, T] =>
        dispatchInitialStateSetTraitElements(e)
      case e: StateHashBuilderElements[S, T] =>
        dispatchStateHashBuilderElement(e)
      case e: FinalStateSetHashBuilderElements[S, T] =>
        dispatchFinalStateSetHashBuilderElement(e)
      case AddTransition(state1, trans, state2) =>
        addTransition(state1, trans, state2)
      case RemoveTransition(state, trans, state2) =>
        removeTransition(state, trans, state2)
      case AddETransition(state1, state2) => addETransition(state1, state2)
      case RemoveETransition(state, state2) => removeETransition(state, state2)
    }
}
