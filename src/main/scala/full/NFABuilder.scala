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
import scala.collection.mutable.{HashMap, HashSet}
import org.typelevel.paiges.Doc
import org.maraist.fa.styles.AutomatonStyle
import org.maraist.fa.traits
import org.maraist.fa.elements.*

/** Implementation of [[org.maraist.fa.traits.NFABuilder NFABuilder]]
  * using [[scala.collection.mutable.HashMap `HashMap`s]] and
  * [[scala.collection.mutable.HashSet `HashSet`s]].
  * @constructor Returns a builder holding an initially empty automaton
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  *
  * @group NFA
  */
trait NFABuilder[
  S, T,
  G[X] <: Set[X],
  +D[X, Y] <: DFA[X, Y, DZ],
  +N[X, Y] <: NFA[X, Y, G, D, NZ, DZ],
  -K >: NFAelements[S,T] <: Matchable,
  -NZ[X, Y] <: AutomatonStyle[X, Y],
  -DZ[X, Y] <: AutomatonStyle[X, Y]
]

extends traits.NFABuilder[S, T, G, D, N, K, NZ, DZ]

with FABuilder[S, T, N, K, NZ] {

  protected var initialStatesVar = new HashSet[S]
  protected val transitionsMap = new HashMap[S, HashMap[T, HashSet[S]]]
  protected val epsilons = new HashMap[S, HashSet[S]]

  def addTransition(s1: S, t: T, s2: S): Unit = {
    if !isState(s1) then {
      throw new IllegalArgumentException(
        s"Transition source $s1 is not a known state")
    }
    if !isState(s2) then {
      throw new IllegalArgumentException(
        s"Transition target $s2 is not a known state")
    }

    val submap = transitionsMap.get(s1) match {
      case Some(m) => m
      case None => {
        val newMap = HashMap(t -> HashSet.empty[S])
        transitionsMap(s1) = newMap
        newMap
      }
    }

    val set = submap.get(t) match {
      case Some(s) => s
      case None => {
        val newSet = HashSet.empty[S]
        submap(t) = newSet
        newSet
      }
    }

    set += s2
  }

  def removeTransition(s1: S, t: T, s2: S): Unit =
    transitionsMap.get(s1) match {
      case None => { }
      case Some(m) => {
        m.get(t) match {
          case None => { }
          case Some(s) => {
            s -= s2
            if (s.size == 0) { m -= t  }
          }
        }
        if (m.size == 0) { transitionsMap -= s1 }
      }
    }

  def addETransition(s1: S, s2: S): Unit = {
    if !isState(s1) then {
      throw new IllegalArgumentException(
        s"Transition source $s1 is not a known state")
    }

    if !isState(s2) then {
      throw new IllegalArgumentException(
        s"Transition target $s2 is not a known state")
    }

    val set = epsilons.get(s1) match {
      case Some(s) => s
      case None => {
        val newSet = HashSet.empty[S]
        epsilons(s1) = newSet
        newSet
      }
    }

    set += s2
  }

  def removeETransition(s1: S, s2: S): Unit =
    epsilons.get(s1) match {
      case None => { }
      case Some(set) => {
        set -= s2
        if (set.size == 0) { epsilons -= s1 }
      }
    }

  def addInitialState(s: S): Unit = {
    addState(s)
    initialStatesVar += s
  }

  def removeInitialState(s: S): Unit = { initialStatesVar -= s }

  def accepts(string: Seq[T]): Boolean = toDFA.accepts(string)

  def foreachETransition(action: (S, S) => Unit): Unit =
    for ((s1, s2s) <- epsilons; s2 <- s2s)
      do action(s1, s2)

  override def eTransitionPairs: Iterable[(S, S)] =
    for ((s1, s2s) <- epsilons; s2 <- s2s)
      yield (s1, s2)

  def initialStates: Set[S] = initialStatesVar.toSet

  def isInitialState(s: S): Boolean = initialStatesVar.contains(s)

  def labels: Iterable[T] =
    (for ((_, map) <- transitionsMap; (t, _) <- map) yield t).toSet

  def transitions(s: S, t: T): Set[S] =
    transitionsMap.get(s).flatMap(_.get(t)).map(_.toSet).getOrElse(Set.empty[S])

  def eTransitions(s: S): Set[S] =
    epsilons.get(s).map(_.toSet).getOrElse(Set.empty[S])

  override def toDFA: D[G[S],T] = result().toDFA

  protected def deleteTransitionsFrom(s:S) = {
    transitionsMap -= s
    for(lmap <- transitionsMap.valuesIterator)
      for(v <- for(v <- lmap.keysIterator if (lmap(v).equals(s))) yield v)
        lmap -= v
    epsilons -= s
    for(v <- epsilons.valuesIterator) v -= s
  }

  private def slowFindIndex[X](seq: IndexedSeq[X], x: X): Int = {
    import scala.util.control.NonLocalReturns.*
      returning {
        for(i <- 0 until seq.size) do {
          // println(s" && [&i]\n    ${seq(i)}\n    $x")
          if seq(i) == x then throwReturn(i)
        }
        -1
      }
  }

  def nfaComponents: (
    IndexedSeq[S], IndexedSeq[T], HashSet[Int],
    Array[Array[Set[Int]]], Array[Set[Int]]
  ) = {
    val statesSeq: IndexedSeq[S] = IndexedSeq.from(allStates)
    val transitionsSeq: IndexedSeq[T] = IndexedSeq.from(labels)
    val finals: HashSet[Int] = new HashSet[Int]
    for(s <- finalStatesSet) finals += slowFindIndex(statesSeq, s) // statesSeq.indexOf(s)
    val empty = Set.empty[Int]

    // Allocate space for the NFA's array lookups.
    val labelsArray =
      Array.ofDim[Set[Int]](statesSeq.size, transitionsSeq.size)
    val epsilonsArray = Array.ofDim[Set[Int]](statesSeq.size)

    // statesSeq.map((s) => println(s"# " + s))

    // Fill out the arrays, starting with source state.
    for(si:Int <- 0 until statesSeq.length) {
      val s:S = statesSeq(si)


      for(ti:Int <- 0 until transitionsSeq.length) {
        val t:T = transitionsSeq(ti)
        labelsArray(si)(ti) =
          transitionsMap.get(s) match {
            case Some(curry) => {
              // println(s" -- ($si)($ti) ==> $curry, ${curry.map(statesSeq.indexOf(_))}")
              // curry.get(t).fold(empty)(_.map(statesSeq.indexOf(_)).toSet)
              // println(s" -- ($si)($ti) ==> $curry, ${curry.map(slowFindIndex(statesSeq, _))}")
              val maybeDests: Option[HashSet[S]] = curry.get(t)
              val dests: Set[S] = maybeDests.map(_.toSet).getOrElse(Set.empty)
              dests.map(slowFindIndex(statesSeq, _))
            }
            case None => empty
          }
        }

      epsilonsArray(si) =
        epsilons.get(s).fold(empty)(_.map(slowFindIndex(statesSeq, _)).toSet)
    }

    (statesSeq, transitionsSeq, finals, labelsArray, epsilonsArray)
  }

  /** Creates an immutable [[NFA NFA]] corresponding to the automaton
    * described to this builder.  This method creates elements of the
    * internal state of the NFA, and passes them to the abstract
    * method [[#assembleNFA]]: the actual definition of
    * [[#assembleNFA]] is in the concrete classes (of package
    * [[org.maraist.fa]]) implementing this trait.
    */
  def result(): N[S, T] = {
    val (statesSeq, transitionsSeq, finals, labelsArray, epsilonsArray) =
      nfaComponents

    assembleNFA(
      statesSeq,
      initialStatesVar.map(statesSeq.indexOf(_)).toSet,
      finals.toSet,
      transitionsSeq, labelsArray,
      epsilonsArray)
  }

  protected def assembleNFA(
    statesSeq: IndexedSeq[S],
    initials: Set[Int],
    finals: Set[Int],
    transitionsSeq: IndexedSeq[T],
    labelsArray: Array[Array[Set[Int]]],
    epsilonsArray: Array[Set[Int]]):
      N[S, T]

  /** Helper method for the [[scala.collection.mutable.Builder]]
    * implementation.
    */
  override def addOne(builder: K): this.type = {
    builder match {
      case AddInitialState(s): AddInitialState[S] => addInitialState(s)
      case RemoveInitialState(s): RemoveInitialState[S] => removeInitialState(s)
      case AddTransition(s0, t, s1): AddTransition[S, T] =>
        addTransition(s0, t, s1)
      case RemoveTransition(s0, t, s1): RemoveTransition[S, T] =>
        removeTransition(s0, t, s1)
      case AddETransition(s0, s1): AddETransition[S] =>
        addETransition(s0, s1)
      case RemoveETransition(s0, s1): RemoveETransition[S] =>
        removeETransition(s0, s1)
      case _ => super.addOne(builder)
    }
    this
  }

  override def map[S2, T2](stateMap: S => S2, transitionMap: T => T2):
      NFA[S2, T2, G, D, NZ, DZ] = {
    val (statesSeq, transitionsSeq, finals, labelsArray, epsilonsArray) =
      nfaComponents
    derivedNFA(
      statesSeq.map(stateMap), transitionsSeq.map(transitionMap),
      initialStatesVar.map(statesSeq.indexOf(_)).toSet,
      finals.toSet, labelsArray, epsilonsArray)
  }

  override def mapStates[S2](stateMap: S => S2): NFA[S2, T, G, D, NZ, DZ] =
    map(stateMap, (t: T) => t)

  override def mapTransitions[T2](transitionMap: T => T2):
      NFA[S, T2, G, D, NZ, DZ] =
    map((s: S) => s, transitionMap)

  /** Internal method for instantiating a DFA of the appropriate runtime
    * type which may not have the same type components as this DFA.
    */
  protected def derivedNFA[S0, T0](
    stateSeq: IndexedSeq[S0],
    transitionsSeq: IndexedSeq[T0],
    initials: Set[Int],
    finalStateIndices: Set[Int],
    labelsArray: Array[Array[Set[Int]]],
    epsilonsArray: Array[Set[Int]]
  ): N[S0, T0]

  override protected def prettyHeader: Doc =
    Doc.text("---------- NFABuilder dump")
}
