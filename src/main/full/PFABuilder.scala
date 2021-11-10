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
import scala.collection.mutable.HashSet
import org.maraist.fa.{traits,util}
import org.maraist.fa.elements.{PFAelements,
  AddState, RemoveState, AddProbFinalState, RemoveFinalState,
  AddProbTransition, RemoveTransition, SetInitialState, AddProbETransition,
  RemoveProbETransition}
import org.maraist.fa.styles.ProbabilisticAutomatonStyle

/** Implementation of [[org.maraist.fa.traits.PFABuilder PFABuilder]]
 *  using [[scala.collection.mutable.HashSet `HashSet`s]] and
 *  [[scala.collection.mutable.HashMap `HashMap`s]].
 * @param initialState The initial state of the automaton.  This builder must
 * be instantiated with one state, initially taken to be the initial state.
 * @tparam S The type of all states of the automaton
 * @tparam T The type of labels on (non-epsilon) transitions of the automaton
 *
 * @group DFA
 */
trait PFABuilder[
  S, T,
  +A[AS, AT] <: PFA[AS, AT, Z],
  -K >: PFAelements[S, T] <: Matchable,
  -Z[ZS, ZT] <: ProbabilisticAutomatonStyle[ZS, ZT]]

extends traits.PFABuilder[S, T, A, K, Z]

with StatesMixin[S, T] with UnindexedPFA[S, T, Z] {

  protected val transitionsMap = new HashMap[S, HashMap[T, HashMap[S, Double]]]
  protected val eTransitionsMap = new HashMap[S, HashMap[S, Double]]
  protected val initialProb = new HashMap[S, Double]
  protected val finalProb = new HashMap[S, Double]

  override def clear(): Unit = {
    super.clear()
    transitionsMap.clear()
    eTransitionsMap.clear()
    initialProb.clear()
    finalProb.clear()
  }

  def eTransitionProb(s0: S, s1: S): Double =
    eTransitionsMap.get(s0).flatMap(_.get(s1)).getOrElse(0.0)

  def foreachETransition(action: (S, S, Double) => Unit): Unit =
    for ((s0, curry) <- eTransitionsMap; (s1, prob) <- curry; if prob > 0.0)
      do action(s0, s1, prob)

  override def eTransitionPairs: Iterable[(S, S)] =
    for ((s0, curry) <- eTransitionsMap; (s1, prob) <- curry; if prob > 0.0)
      yield (s0, s1)

  def foreachFinalState(action: (S, Double) => Unit): Unit =
    for ((s, prob) <- finalProb; if prob > 0.0) do action(s, prob)

  def foreachInitialState(action: (S, Double) => Unit): Unit =
    for ((s, prob) <- initialProb; if prob > 0.0) do action(s, prob)

  def foreachState(action: (S, Double) => Unit): Unit =
    for (s <- states) do action(s, initialProb.getOrElse(s, 0.0))

  def foreachTransition(action: (S, T, S, Double) => Unit): Unit =
  for (
    (s0, curry0) <- transitionsMap;
    (t, curry1) <- curry0;
    (s1, prob) <- curry1;
    if prob > 0.0
  )
    do action(s0, t, s1, prob)

  def transitionProb(s0: S, t: T, s1: S): Double =
    transitionsMap.get(s0).flatMap(_.get(t)).flatMap(_.get(s1)).getOrElse(0.0)

  def initialStateProb(s: S): Double = initialProb.get(s) match {
    case Some(p) => p
    case None => 0.0
  }

  def finalStateProb(s: S): Double = finalProb.get(s) match {
    case Some(p) => p
    case None => 0.0
  }

  def addInitialState(s: S, prob: Double): Unit = {
    addState(s)
    initialProb(s) = prob
  }

  def removeInitialState(s: S): Unit = { initialProb.remove(s) }

  def addFinalState(s: S, prob: Double): Unit = {
    addState(s)
    finalProb(s) = prob
  }

  def removeFinalState(s: S): Unit = { finalProb.remove(s) }

  private[fa] def deleteInitialState(s: S): Unit = { initialProb.remove(s) }
  private[fa] def deleteFinalState(s: S): Unit = { finalProb.remove(s) }
  protected def deleteTransitionsFrom(s: S) = {
    transitionsMap -= s
    for(lmap: HashMap[T,HashMap[S,Double]] <- transitionsMap.valuesIterator)
      for(v <- lmap.keysIterator)
        if (lmap(v).contains(s)) {
          lmap(v) -= s
          if (lmap.isEmpty) { lmap -= v }
        }
  }

  def addTransition(s1: S, t: T, s2: S, prob: Double): Unit = {
    allStates += s1
    allStates += s2
    if (!transitionsMap.contains(s1)) {
      transitionsMap += (s1 -> new HashMap[T,HashMap[S,Double]])
    }

    val sub1: HashMap[T, HashMap[S, Double]] = transitionsMap(s1)
    if (!sub1.contains(t)) {
      sub1 += (t -> new HashMap[S,Double])
    }

    val sub2: HashMap[S,Double] = sub1(t)
    sub2 += (s2 -> prob)
  }
  def removeTransition(s1: S, t: T, s2: S): Unit =
    if (transitionsMap.contains(s1))
      transitionsMap(s1) -= t

  def transition(s0: S, t: T, s1: S): Double = {
    if (transitionsMap.contains(s0)) {
      val sub: HashMap[T,HashMap[S,Double]] = transitionsMap(s0)
      if (sub.contains(t)) {
        val sub2 = sub(t)
        if (sub2.contains(s1)) sub2(s1) else 0.0
      } else 0.0
    } else 0.0
  }

  def eTransition(s0: S, s1: S): Double = {
    eTransitionsMap.get(s0) match {
      case Some(submap) => {
        submap.get(s1) match {
          case Some(p) => p
          case None => 0.0
        }
      }
      case None => 0.0
    }
  }

  def addETransition(s1: S, s2: S, prob: Double): Unit = {
    eTransitionsMap.get(s1) match {
      case Some(submap) => { submap(s2) = prob }
      case None => {
        val submap = new HashMap[S,Double]
        submap(s2) = prob
        eTransitionsMap(s1) = submap
      }
    }
  }

  def removeETransition(s1: S, s2: S): Unit = {
    eTransitionsMap.get(s1) match {
      case Some(submap) => { submap.remove(s2) }
      case None => { }
    }
  }

  def transition(s0: S, t: T): Map[S,Double] = {
    val result = new HashMap[S,Double]()
    if (transitionsMap.contains(s0)) {
      val sub: HashMap[T,HashMap[S,Double]] = transitionsMap(s0)
      if (sub.contains(t)) {
        for ((s1,prob) <- sub(t)) {
          if (prob>0.0)
            result(s1) = prob
        }
      }
    }
    result.toMap
  }

  def labels: Set[T] = {
    val result = new HashSet[T]()
    for(map <- transitionsMap.valuesIterator)
      for(t <- map.keysIterator)
        result += t
    result.toSet
  }

  import scala.util.control.NonLocalReturns.*
  override def acceptsProb(ts: Seq[T]): Double = returning {
    var current = List.from(for(s<-initialProb.keys) yield (initialProb(s),s))
    for(t <- ts) {
      val oldCurrent = current
      current = List[(Double,S)]()
      for ((p0,s0) <- oldCurrent) {
        for ((s1,p1) <- transition(s0,t)) {
          if (p1>0.0) {
            current = (p0*p1,s1) :: current
          }
        }
      }
      if (current.size == 0) throwReturn(0.0)
    }

    var result: Double = 0.0
    for ((p,s) <- current) { result += p }
    result
  }

  def normalize: Unit = { // scalastyle: ignore cyclomatic.complexity
    val totalOut = new HashMap[S,Double]
    var totalInit: Double = 0.0
    for(s1 <- allStates) {
      totalInit += initialStateProb(s1)
      totalOut(s1) = finalStateProb(s1)
      for(s2 <- allStates) {
        totalOut(s1) += eTransition(s1,s2)
        for(t <- labels)
          totalOut(s1) += transition(s1,t,s2)
      }
    }
    for(s1 <- allStates) {
      if (totalInit>0.0) initialProb.get(s1) match {
        case Some(p) => { initialProb(s1) = p/totalInit }
        case None => { }
      }
      if (totalOut(s1) > 0.0) {
        finalProb.get(s1) match {
          case Some(p) => { finalProb(s1) = p/totalOut(s1) }
          case None => { }
        }
        transitionsMap.get(s1) match {
          case Some(s1submap) => {
            for((t,tSubmap) <- s1submap)
              for((s2,p) <- tSubmap)
                transitionsMap(s1)(t)(s2) = p / totalOut(s1)
          }
          case None => { }
        }
        eTransitionsMap.get(s1) match {
          case Some(s1submap) => {
            for((s2,p) <- s1submap)
              s1submap(s2) = p / totalOut(s1)
          }
          case None => { }
        }
      }
    }
  }

  override def removeEpsilonTransitions: Unit =
    new util.PFAEpsilonRemover(this).run()

  override def addOne(builder: K): this.type = {
    builder match {
      case AddState(s): AddState[S, T] => addState(s)
      case RemoveState(state): RemoveState[S, T] => removeState(state)
      case AddProbFinalState(state, prob): AddProbFinalState[S, T] =>
        addFinalState(state, prob)
      case RemoveFinalState(state): RemoveFinalState[S, T] =>
        removeFinalState(state)
      case AddProbTransition(state1, trans, state2, prob):
          AddProbTransition[S, T] =>
        addTransition(state1, trans, state2, prob)
      case RemoveTransition(state1, trans, state2): RemoveTransition[S, T] =>
        removeTransition(state1, trans, state2)
      case SetInitialState(state): SetInitialState[S] =>
        SetInitialState(state)
      case AddProbETransition(state1, state2, prob): AddProbETransition[S, T] =>
        addETransition(state1, state2, prob)
      case RemoveProbETransition(state1, state2, prob):
          RemoveProbETransition[S, T] =>
        removeETransition(state1, state2)
    }
    this
  }

  def result(): A[S, T] = { // scalastyle: ignore cyclomatic.complexity method.length
    val statesSeq: IndexedSeq[S] = IndexedSeq.from(allStates)
    val transitionsSeq: IndexedSeq[T] = IndexedSeq.from(labels)
    val initialProbs = Array.ofDim[Double](statesSeq.length)
    val finalProbs = Array.ofDim[Double](statesSeq.length)
    val totalOut = Array.ofDim[Double](statesSeq.length)
    val idxLabels: Array[Array[Array[Double]]] =
      Array.ofDim[Double](statesSeq.length, transitionsSeq.length,
                          statesSeq.length)
    val eLabels: Array[Array[Double]] = Array.ofDim[Double](statesSeq.length,
                                                           statesSeq.length)
    var totalInit = 0.0
    for(si0 <- 0 until statesSeq.length) {
      val s0: S = statesSeq(si0)

      val thisInit  = initialStateProb(s0)
      val thisFinal = finalStateProb(s0)
      initialProbs(si0) = thisInit
      totalInit += thisInit
      finalProbs(si0) = thisFinal
      totalOut(si0) = thisFinal

      if (transitionsMap.contains(s0)) {
        val s0map = transitionsMap(s0)

        for(ti <- 0 until transitionsSeq.length) {
          val t: T = transitionsSeq(ti)
          if (s0map.contains(t)) {
            val tMap = s0map(t)

            for(si1 <- 0 until statesSeq.length) {
              val s1: S = statesSeq(si1)
              if (tMap.contains(s1)) {
                idxLabels(si0)(ti)(si1) = tMap(s1)
                totalOut(si0) += tMap(s1)
              } else {
                idxLabels(si0)(ti)(si1) = 0.0
              }
            }
          } else {
            for(si1 <- 0 until statesSeq.length)
              idxLabels(si0)(ti)(si1) = 0.0
          }
        }
      } else {
        for(ti <- 0 until transitionsSeq.length)
          for(si1 <- 0 until statesSeq.length)
            idxLabels(si0)(ti)(si1) = 0.0
      }
    }

    // Now normalize the weights for the final array
    for(si0 <- 0 until statesSeq.length) {
      if (totalInit>0.0) { initialProbs(si0) = initialProbs(si0)/totalInit }
      val out = totalOut(si0)
      if (out > 0.0) {
        finalProbs(si0) = finalProbs(si0) / out
        for(ti <- 0 until transitionsSeq.length)
          for(si1 <- 0 until statesSeq.length)
            idxLabels(si0)(ti)(si1) = idxLabels(si0)(ti)(si1) / out
      }
    }

    assemblePFA(statesSeq, initialProbs, finalProbs, transitionsSeq,
                idxLabels, eLabels)
  }

  override def map[S2, T2](stateMap: S => S2, transitionMap: T => T2):
      PFA[S2, T2, Z] = result.map(stateMap, transitionMap)

  override def mapStates[S2](stateMap: S => S2): PFA[S2, T, Z] =
    map(stateMap, (t: T) => t)

  override def mapTransitions[T2](transitionMap: T => T2): PFA[S, T2, Z] =
    map((s: S) => s, transitionMap)

  protected def assemblePFA(statesSeq: IndexedSeq[S],
                            initialProbs: Array[Double],
                            finalProbs: Array[Double],
                            transitionsSeq: IndexedSeq[T],
                            idxLabels: Array[Array[Array[Double]]],
                            eTransLabels: Array[Array[Double]]): A[S, T]
}
