// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

/** Partial implementation of a builder for DFAs using
  * [[scala.collection.mutable.HashSet `HashSet`s]] and
  * [[scala.collection.mutable.HashMap `HashMap`s]].
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  * @tparam D Type of DFA constructed by this builder.
  * @tparam K Builder elements for this builder.
  * @tparam Z Type of style options for Graphviz export
  *
  * @group DFA
  */
package org.maraist.fa.full
import scala.collection.mutable.{HashMap, HashSet}
import org.maraist.fa.elements.*
import org.maraist.fa.styles.AutomatonStyle
import org.maraist.fa.traits

trait DFABuilder[
  S, T,
  +D[S, T] <: DFA[S, T, Z],
  K >: DFAelements[S, T] <: Matchable,
  -Z[X, Y] <: AutomatonStyle[X, Y]
](initState: S)

extends traits.DFABuilder[S, T, D, K, Z]
    with UnindexedDFA[S, T, Z]
    with FABuilder[S, T, D, K, Z] {

  protected var initialStateVar: S = initState

  protected val transitionsMap = new HashMap[S, HashMap[T, S]]

  def result(): D[S, T] = {
    val statesSeq: IndexedSeq[S] = IndexedSeq.from(allStates)
    val transitionsSeq: IndexedSeq[T] = IndexedSeq.from(labels)
    val initialIdx: Int = statesSeq.indexOf(initialState)
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
                            idxLabels: Array[Array[Int]]): D[S, T]

  override def initialState: S = initialStateVar

  override def setInitialState(s: S): Unit = {
    addState(s)
    initialStateVar = s
  }

  override def isInitialState(s: S): Boolean =
    initialStateVar.equals(s)

  override def addTransition(s1: S, t: T, s2: S): Unit = {
    addState(s1)
    addState(s2)
    if (!transitionsMap.contains(s1)) {
      transitionsMap += (s1 -> new HashMap[T,S])
    }
    transitionsMap(s1) += (t -> s2)
  }

  override def removeTransition(s1: S, t: T): Unit =
    if (transitionsMap.contains(s1))
      transitionsMap(s1) -= t

  override def transition(s: S, t: T): Option[S] = {
    if (transitionsMap.contains(s)) {
      val sub:HashMap[T,S] = transitionsMap(s)
      sub.get(t)
    } else None
  }

  override protected def deleteTransitionsFrom(s:S) = {
    transitionsMap -= s
    for(lmap <- transitionsMap.valuesIterator)
      for(v <- for(v <- lmap.keysIterator if (lmap(v).equals(s))) yield v)
        lmap -= v
  }

  override def labels: Set[T] = {
    val result = new HashSet[T]()
    for(map <- transitionsMap.valuesIterator)
      for(t <- map.keysIterator)
        result += t
    result.toSet
  }

  override def addOne(elem: K): this.type = {
    elem match {
      case SetInitialState(s): SetInitialState[S] =>
        setInitialState(s)
      case AddTransition(state1, trans, state2): AddTransition[S, T] =>
        addTransition(state1, trans, state2)
      case RemoveTransition(state, trans, state2): RemoveTransition[S,T] =>
        removeTransition(state, trans)
      case _ => super.addOne(elem)
    }
    this
  }

  override protected def dumpHeader(): Unit =
    println("---------- DFABuilder dump")
}
