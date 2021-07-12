// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.general
import scala.collection.mutable.{HashMap,HashSet}
import java.awt.geom.GeneralPath
import org.maraist.fa.general.Builders.*
import org.maraist.fa.traits.{
  StateAdder, StateHolder, StateBuilder,
  FinalStateSetHolder, FinalStateSetBuilder,
  SingleInitialStateHolder, SingleInitialStateBuilder
}

/** Mixin of builder routines pertaining to states for
 * [[org.maraist.fa.Automaton Automaton]]s using
 * [[scala.collection.mutable.HashSet `HashSet`s]] and
 * [[scala.collection.mutable.HashMap `HashMap`s]].
 * @param initialState The initial state of the automaton.  This builder must
 * be instantiated with one state, initially taken to be the initial state.
 * @tparam S The type of all states of the automaton
 * @tparam T The type of labels on (non-epsilon) transitions of the automaton
 *
 * @group General
 */
trait HashSetStateBuilderMixin[S,T]
    extends StateHolder[S] with StateBuilder[S] {

  /** Storage for all state objects */
  protected val allStates:HashSet[S] = new HashSet[S]

  private[fa] def deleteTransitionsFrom(s:S): Unit
  private[fa] def deleteFinalState(s:S): Unit
  private[fa] def deleteInitialState(s:S): Unit

  def addState(s:S):Unit = { allStates += s }
  def removeState(s:S):Unit = {
    allStates -= s
    deleteFinalState(s)
    deleteInitialState(s)
    deleteTransitionsFrom(s)
  }
  def size:Int = allStates.size
  def states:Set[S] = Set.from(allStates)
  def isState(s:S):Boolean = allStates.contains(s)

  protected def dispatchStateBuilderElement
    (elem: StateBuilderElement[S, T]):
      Unit = elem match {
    case AddState(s) => addState(s)
    case RemoveState(s) => removeState(s)
  }
}

type StateBuilderElement[S, T] = AddState[S,T] | RemoveState[S,T]

/** Implementation of builder routines pertaining to final states for
 * [[org.maraist.fa.Automaton Automaton]]s using
 * [[scala.collection.mutable.HashSet `HashSet`s]] and
 * [[scala.collection.mutable.HashMap `HashMap`s]].
 * @param initialState The initial state of the automaton.  This builder must
 * be instantiated with one state, initially taken to be the initial state.
 * @tparam S The type of all states of the automaton
 * @tparam T The type of labels on (non-epsilon) transitions of the automaton
 *
 * @group General
 */
trait HashFinalStateSetBuilderMixin[S,T]
    extends FinalStateSetHolder[S] with FinalStateSetBuilder[S] {
  this: StateAdder[S] =>

  /** Storage for all final state objects */
  protected val finalStatesSet: HashSet[S] = new HashSet[S]

  private[fa] def deleteFinalState(s:S):Unit = { finalStatesSet -= s }

  def addFinalState(s: S): Unit = {
    finalStatesSet += s
    addState(s)
  }
  def removeFinalState(s: S): Unit = { finalStatesSet -= s }

  def finalStates: Set[S] = Set.from(finalStatesSet)
  def isFinalState(s:S):Boolean = finalStatesSet.contains(s)

  protected def dispatchFinalStateSetHashBuilderElement
    (elem: FinalStateSetBuilderElement[S, T]):
      Unit = elem match {
    case AddFinalState(s) => addFinalState(s)
    case RemoveFinalState(s) => removeFinalState(s)
  }
}

type FinalStateSetBuilderElement[S, T] =
  AddFinalState[S,T] | RemoveFinalState[S,T]

/**
  * Mixin of builder routines for
  * [[org.maraist.fa.Automaton Automaton]]s pertaining to the initial
  * state using [[scala.collection.mutable.HashSet `HashSet`s]] and
  * [[scala.collection.mutable.HashMap `HashMap`s]].
  * @param initialState The initial state of the automaton.  This builder must
  * be instantiated with one state, initially taken to be the initial state.
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  *
  * @group General
  */
abstract class SingleInitialStateMixin[S,T]
  (protected var initialStateVar: S)
    extends SingleInitialStateHolder[S] with SingleInitialStateBuilder[S] {
  this: StateAdder[S] =>

  def getInitialState: S = initialStateVar

  def setInitialState(s:S):Unit = {
    addState(s)
    initialStateVar = s
  }
  def isInitialState(s:S):Boolean = initialStateVar.equals(s)
  private[fa] def deleteInitialState(s:S):Unit =
    throw new IllegalStateException()

  protected def dispatchSingleInitialStateMixinElement
    (elem: SingleInitialStateMixinElement[S, T]):
      Unit = elem match {
    case SetInitialState(s) => setInitialState(s)
  }
}

type SingleInitialStateMixinElement[S, T] = SetInitialState[S]

/** Mixin behavior for an automaton with a single initial state.
  *
  * @group DFA
  */
trait InitialStateSetTrait[S,T] {
  this: StateAdder[S] =>

  private val initialStatesSet = new HashSet[S]
  def addInitialState(s:S):Unit = {
    addState(s)
    initialStatesSet += s
  }
  def removeInitialState(s:S):Unit = {
    initialStatesSet -= s
  }
  def isInitialState(s:S):Boolean = initialStatesSet.contains(s)
  def getInitialStates: Set[S] = initialStatesSet.toSet
  private[fa] def deleteInitialState(s:S):Unit = {
    removeInitialState(s)
  }

  protected def dispatchInitialStateSetTraitElements
    (elem: InitialStateSetTraitElements[S, T]):
      Unit = elem match {
    case AddInitialState(s) => AddInitialState(s)
    case RemoveInitialState(s) => RemoveInitialState(s)
  }
}

type InitialStateSetTraitElements[S, T] =
  AddInitialState[S] | RemoveInitialState[S]

trait DeterministicLabelledTransitionMixin[S, T] {
  this: StateAdder[S] =>

  protected val transitionsMap = new HashMap[S,HashMap[T,S]]

  def addTransition(s1: S, t: T, s2: S): Unit = {
    addState(s1)
    addState(s2)
    if (!transitionsMap.contains(s1)) {
      transitionsMap += (s1 -> new HashMap[T,S])
    }
    transitionsMap(s1) += (t -> s2)
  }

  def removeTransition(s1:S, t:T):Unit =
    if (transitionsMap.contains(s1))
      transitionsMap(s1) -= t

  def transition(s:S, t:T):Option[S] = {
    if (transitionsMap.contains(s)) {
      val sub:HashMap[T,S] = transitionsMap(s)
      sub.get(t)
    } else None
  }

  private[fa] def deleteTransitionsFrom(s:S) = {
    transitionsMap -= s
    for(lmap <- transitionsMap.valuesIterator)
      for(v <- for(v <- lmap.keysIterator if (lmap(v).equals(s))) yield v)
        lmap -= v
  }

  def labels: Set[T] = {
    val result = new HashSet[T]()
    for(map <- transitionsMap.valuesIterator)
      for(t <- map.keysIterator)
        result += t
    result.toSet
  }

  protected def dispatchDeterministicLabelledTransitionMixinElement
    (elem: DeterministicLabelledTransitionMixinElement[S, T]):
      Unit = elem match {
    case AddTransition(state1, trans, state2) =>
      addTransition(state1, trans, state2)
    case RemoveTransition(state, trans, state2) =>
      removeTransition(state, trans)
  }
}

type DeterministicLabelledTransitionMixinElement[S, T] =
  AddTransition[S, T] | RemoveTransition[S, T]

trait NondeterministicLabelledTransitionMixin[S, T] {
  this: StateAdder[S] =>

  /** Maps from a state `s` and label `t` to the set of states at the
    * end of transitions starting from `s` and labeled `t` */
  protected val transitionsMap = new HashMap[S,HashMap[T,HashSet[S]]]

  def addTransition(s1:S, t:T, s2:S):Unit = {
    addState(s1)
    addState(s2)
    val submap:HashMap[T,HashSet[S]] =
      transitionsMap.getOrElseUpdate(s1,new HashMap[T,HashSet[S]])
    val subsubmap:HashSet[S] = submap.getOrElseUpdate(t, new HashSet[S])
    subsubmap += s2
  }

  def removeTransition(s1:S, t:T, s2:S):Unit = {
    if (transitionsMap.contains(s1)) {
      val submap:HashMap[T,HashSet[S]] = transitionsMap(s1)
      if (submap.contains(t)) submap(t) -= s2
    }
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
}

type NondeterministicLabelledTransitionMixinElements[S, T] =
  AddTransition[S, T] | RemoveTransition[S, T]
