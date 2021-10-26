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
import org.maraist.fa.styles.ProbabilisticAutomatonStyle
import org.maraist.fa.traits

/** Implementation of a [[org.maraist.fa.DFA DFA]] using
 *  [[scala.collection.immutable.IndexedSeq `IndexedSeq`s]] and
 *  `Array`s.
 * @param initialStateIndex Index of the initial state of the automaton.
 * @param finalStateIndices Set of the indices of the final states of the
 * automaton
 * @tparam S The type of all states of the automaton
 * @tparam T The type of labels on (non-epsilon) transitions of the automaton
 *
 * @group PFA
 */
trait PFA[S, T, -Z[S, T] <: ProbabilisticAutomatonStyle[S, T]]

extends traits.PFA[S, T, Z]

with UnindexedPFA[S, T, Z] {

  protected def stateSeq: IndexedSeq[S]
  protected def initialProbs: Array[Double]
  protected def finalProbs: Array[Double]
  protected def transitionsSeq: IndexedSeq[T]
  protected def transitionsMatrix: Array[Array[Array[Double]]]
  protected def eTransitionsMatrix: Array[Array[Double]]

  override val indexOf: Map[S, Int] = {
    val builder = Map.newBuilder[S, Int]
    for (i <- 0 until stateSeq.length) do builder += ((stateSeq(i), i))
    builder.result
  }

  override val labelIndex: Map[T, Int] = {
    val builder = Map.newBuilder[T, Int]
    for (i <- 0 until transitionsSeq.length)
      do builder += ((transitionsSeq(i), i))
    builder.result
  }

  override def size: Int = stateSeq.length

  override def states: IndexedSeq[S] = stateSeq

  override def labels: IndexedSeq[T] = transitionsSeq

  override def label(i: Int): T = transitionsSeq(i)

  override def getLabelIndex(t: T): Option[Int] = labelIndex.get(t)

  override def state(i: Int): S = stateSeq(i)

  override def getIndexOf(s: S): Option[Int] = indexOf.get(s)

  override def isState(s: S): Boolean = indexOf.contains(s)

  override def initialStateIndexProb(s: Int): Double = initialProbs(s)

  override def finalStateIndexProb(s: Int): Double = finalProbs(s)

  override val finalStateIndices: Set[Int] =
    (for(i <- 0 until size; if finalProbs(i) > 0.0) yield i).toSet

  override val initialStateIndices: Set[Int] =
    (for(i <- 0 until size; if initialProbs(i) > 0.0) yield i).toSet

  override def eTransitionProb(s0: S, s1: S): Double =
    eTransitionsMatrix(indexOf(s0))(indexOf(s1))

  override def finalStateProb(s: S): Double = finalProbs(indexOf(s))

  override def foreachETransition(action: (S, S, Double) => Unit): Unit =
    for(
      i1 <- 0 until size;
      i2 <- 0 until size;
      prob = eTransitionsMatrix(i1)(i2);
      if prob > 0.0
    ) do action(states(i1), states(i2), prob)

  override def foreachFinalState(action: (S, Double) => Unit): Unit =
    for (i <- finalStateIndices) do action(states(i), finalStateIndexProb(i))

  override def foreachInitialState(action: (S, Double) => Unit): Unit =
    for (i <- initialStateIndices)
      do action(states(i), initialStateIndexProb(i))

  override def foreachState(action: (S, Double) => Unit): Unit =
    for(i <- 0 until size) do action(states(i), initialStateIndexProb(i))

  override def foreachTransition(action: (S, T, S, Double) => Unit): Unit =
    for(
      i1 <- 0 until size;
      ti <- 0 until transitionsSeq.size;
      i2 <- 0 until size;
      prob = transitionsMatrix(i1){ti}(i2);
      if prob > 0.0
    ) do action(states(i1), label(ti), states(i2), prob)

  override def initialStateProb(s: S): Double =
    initialStateIndexProb(indexOf(s))

  override def transitionProb(s0: S, t: T, s1: S): Double =
    transitionsMatrix(indexOf(s0))(labelIndex(t))(indexOf(s1))

  import scala.util.control.NonLocalReturns.*
  override def acceptsProb(ts: Seq[T]): Double = returning {
    var current = List[(Double, Int)]()
    for(s:Int <- 0 until size) {
      val thisProb = initialStateIndexProb(s)
      if (thisProb > 0.0) { current = (thisProb, s) :: current }
    }

    for(t <- ts) {
      val ti = transitionsSeq.indexOf(t)
      val oldCurrent = current
      current = List[(Double,Int)]()
      for ((p0,s0) <- oldCurrent) {
        for ((s1,p1) <- possibleTransitionsIndex(s0, ti)) {
          if (p1>0.0) {
            current = (p0*p1, s1) :: current
          }
        }
      }
      if (current.size == 0) throwReturn(0.0)
    }

    var result: Double = 0.0
    for ((p, s) <- current) do { result += p * finalStateIndexProb(s) }
    result
  }

  override def eTransitionIndexProb(s0: Int, s1: Int): Double =
    eTransitionsMatrix(s0)(s1)

  override def transitionIndexProb(fromIdx: Int, labelIdx: Int, toIdx: Int):
      Double = {
    transitionsMatrix(fromIdx)(labelIdx)(toIdx)
  }

  override def possibleTransitions(s0: S, t: T): Map[S, Double] = {
    val fromIdx = stateSeq.indexOf(s0)
    val labelIdx = transitionsSeq.indexOf(t)
    val idxMap = possibleTransitionsIndex(fromIdx,labelIdx)
    val result = new HashMap[S, Double]
    for ((idx,prob) <- idxMap) {
      result += (stateSeq(idx) -> prob)
    }
    result.toMap
  }

  override def possibleTransitionsIndex(s0: Int, t: Int): Map[Int, Double] = {
    val result = new HashMap[Int, Double]
    val arr = transitionsMatrix(s0)(t)
    for (idx <- 0 to arr.length) {
      if (arr(idx) > 0.0) { result += (idx -> arr(idx)) }
    }
    result.toMap
  }
}
