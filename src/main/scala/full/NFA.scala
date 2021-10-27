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
import scala.collection.mutable.{ArrayBuffer,ListBuffer,HashSet,Queue}
import org.maraist.fa.util.IndexSetsTracker
import org.maraist.fa.traits
import org.maraist.fa.styles.AutomatonStyle

/** Immutable [[org.maraist.fa.traits.NFA NFA]] implementation mixin
  * extending the [[FA][FA mixin]], using
  * [[scala.collection.immutable.IndexedSeq `IndexedSeq`s]] and
  * `Array`s.
  *
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  * @tparam Z Type of style options for Graphviz export
  *
  * @group NFA
  */
trait NFA[
  S, T,
  G[X] <: Set[X],
  +D[DS, DT] <: DFA[DS, DT, DZ],
  -NZ[S, T] <: AutomatonStyle[S, T],
  -DZ[S, T] <: AutomatonStyle[S, T]]

extends traits.NFA[S, T, G, D, NZ, DZ]
    with FA[S, T, NZ] {

  /**
    * IndexedSeq of the actual state objects
    */
  protected val stateSeq: IndexedSeq[S]

  /**
    * Set of indices into `stateSeq` of the final states
    */
  protected val transitionsSeq: IndexedSeq[T]

  /**
    * IndexedSeq of the actual objects used as transition labels
    */
  protected val transitionsArray: Array[? <: Array[? <: Set[Int]]]

  /**
    * IndexedSeq of the actual objects used as transition labels
    */
  protected val epsilonsArray: Array[? <: Set[Int]]

  /**
    * IndexedSeq of the indices of final states.
    */
  val finalStateIndices: Set[Int]

  /**
    * IndexedSeq of the indices of inital states.
    */
  val initialStateIndices: Set[Int]

  // override def state(i: Int): S = stateSeq(i)
  // override def label(i: Int): T = transitionsSeq(i)
  // override def labelIndex(t: T): Int = transitionsSeq.indexOf(t)
  /** Retrieve the index of state `s` */
  // override def indexOf(s: S): Int = stateSeq.indexOf(s)
  override def initialStates: Set[S] = initialStateIndices.map(stateSeq).toSet
  override def finalStates: Set[S] = finalStateIndices.map(stateSeq).toSet
  // override def isState(s: S): Boolean = stateSeq.contains(indexOf(s))
  override def isInitialState(s: S): Boolean =
    initialStateIndices.contains(indexOf(s))
  override def isFinalState(s: S): Boolean =
    finalStateIndices.contains(indexOf(s))

  override def transitions(s: S, t: T): Set[S] = {
    //println(s"\n> transitions(\n  $s,\n  $t\n) --- ${transitionIndices(s,t)}")
    //println(s">> stateSeq --- $stateSeq")
    val result = transitionIndices(s,t).map(stateSeq)
    //println(s">>> transitions(\n  $s,\n  $t\n) $result")
    result
  }
  override def transitionIndices(s: S, t: T): Set[Int] = {
    val si = indexOf(s)
    val ti = labelIndex(t)
    //println(s"    | transitionIndices($s, $t) [$si, $ti]")
    val result = transitionsArray(si)(ti)
    //println(s"    | transitionIndices($s, $t) [$si, $ti] = $result")
    result
  }
  override def eTransitions(s: S): Set[S] = {
    val eti = eTransitionIndices(s)
    // println(eti)
    eti.map(stateSeq)
  }
  override def eTransitionIndices(s: S): Set[Int] = {
    val index = indexOf(s)
    if (index<0) {
      throw new IllegalArgumentException(s.toString() + " not a state in NDFA")
    }
    epsilonsArray(index)
  }

  /** {@inheritDoc} The default implementation is to convert the NFA to
    * a DFA, and check acceptance there.  This is not necessarily
    * good. */
  override def accepts(string: Seq[T]): Boolean = toDFA.accepts(string)

  /** Perform some action for each epsilon transition in the
    * automaton. */
  override def foreachETransition(action: (s1: S, s2: S) => Unit): Unit =
    for(s1 <- states; s2 <- eTransitions(s1))
      do action(s1, s2)

  protected def epsilonCloseIndex(si: Int): (Set[Int], Boolean) =
    epsilonCloseIndices(Set(si))

  protected def epsilonCloseIndices(source: Set[Int]):
      (Set[Int], Boolean) = {
    // println("    * Closing " + source)
    // println("      Epsilons " + epsilons)
    // for (ep <- epsilons) { println("        " + ep) }
    val result = Set.newBuilder[Int]
    var isFinal = false

    for(id <- source) {
      if (finalStateIndices.contains(id)) isFinal=true
      result += id
    }

    val queue:Queue[Int] = Queue.from[Int](source)
    while (!queue.isEmpty) {
      val thisIdx:Int = queue.dequeue()
      // println("    ** "+thisIdx)
      for(newIdx <- epsilonsArray(thisIdx)) {
        // println("    ** ** "+newIdx)
        if (!result.result().contains(newIdx)) {
          result += newIdx
          queue += newIdx
          if (finalStateIndices.contains(newIdx)) isFinal=true
        }
      }
    }
    (result.result(), isFinal)
  }

  def seedAdditionalDfaStates(tracker: IndexSetsTracker): Unit = { }

  override def toDFA: D[G[S], T] = {// scalastyle:ignore cyclomatic.complexity method.length

    // Set up components of ArrayDFA.  We can't convert to a
    // transitions array until the end, so for now we build a map over
    // state/transition label indices.  Note that we just use the same
    // transitions as for this ArrayNDFA.
    val dfaIndexSets: ListBuffer[Set[Int]] = new ListBuffer[Set[Int]]
    val dfaFinals: HashSet[Int] = new HashSet[Int]

    // It will be useful to know the DFA states in which a particular
    // NDFA state appears.
    val appearsIn =
      Array.fill[HashSet[Int]](stateSeq.length)(new HashSet[Int]())

    // Set up tracker for state sets.
    val tracker = new IndexSetsTracker(stateSeq.length, dfaIndexSets)

    // Build initial state.
    val (initialClosure, initialIsFinal) =
      epsilonCloseIndices(initialStateIndices)
    val initialState = tracker.getIndex(initialClosure)
    if (initialState != 0)
      throw new Exception("Unexpected non-zero from first addition to tracker")
    if initialIsFinal then dfaFinals += initialState
    for(initialNda <- initialClosure)
      do appearsIn(initialNda) += initialState

    // Later versions of the NDFA will have hyperedge annotations; we
    // have a thunk here to add more states to the queue.
    seedAdditionalDfaStates(tracker)

    // Iterate through the DFA state list from beginning, adding new
    // states to the end.
    val dfaTransitions = new ArrayBuffer[Array[Int]]
    var nextState = initialState
    val labelCount = transitionsSeq.size
    while(nextState < tracker.size) {
      val s = tracker(nextState)
      val thisRow: Array[Int] = Array.ofDim[Int](labelCount)
      dfaTransitions.insert(nextState, thisRow)

      // The states are the rows of the transitions table; now iterate
      // through the transitions label indices for the columns.
      for (ti:Int <- 0 until labelCount) do {
        val t: T = transitionsSeq(ti)

        // We build the set of states to which this state
        // set/transition pair could lead.
        val destinationsBuilder = Set.newBuilder[Int]
        for(origIndex <- s) {
          val destIdxs = transitionsArray(origIndex)(ti)
          for (destIdx <- destIdxs) do {
            destinationsBuilder += destIdx
          }
        }
        val destinations = destinationsBuilder.result()

        // If the destinations set is empty, then it's an error state.
        if (destinations.isEmpty) {
          // Add an error transition.
          thisRow(ti) = -1

        } else {

          // Close the set under epsilon transitions, cache it, and
          // possibly set it as final.
          epsilonCloseIndices(destinations) match {
            case (closedSet, hasFinal) => {
              val newStateIdx = tracker.getIndex(closedSet)
              dfaEdgeHook(nextState, s, ti, t, closedSet, newStateIdx, tracker)
              for(state <- closedSet) do appearsIn(state) += newStateIdx
              if hasFinal then dfaFinals += newStateIdx

              // Add the new destination index to the action tables.
              thisRow(ti) = newStateIdx
            }
          }
        }
      }
      nextState += 1
    }

    val dfaStates = ArrayBuffer.from(dfaIndexSets.map(_.map(stateSeq(_))))
    val dfaTransArray = Array.from(dfaTransitions)

    // Assemble components
    assembleDFA(
      IndexedSeq.from(dfaStates), 0, dfaFinals.toSet, transitionsSeq,
      dfaTransArray, tracker, appearsIn.map(_.toSet))
  }

  /** This method serves as a hook into the Rabin-Scott implementation
    * for variation by extensions of the classical NFA.  Does nothing
    * by default, but can be overridden in subclasses.  This method is
    * called at the point where an entry is added to the (under
    * construction) array of DFA transitions.
    */
  protected def dfaEdgeHook(
    dfaSrcIdx: Int, nfaSrcIndices: Set[Int],
    ti: Int, t: T,
    nfaDestIndices: Set[Int], dfaDestIdx: Int,
    tracker: IndexSetsTracker):
      Unit = { }

  /**
    * This method is implemented by subclasses where `D[S, T]` is
    * concretized, to provide a DFA implementation of the specific
    * type.
    *
    * @param appearsIn Maps from the index of an NFA state to a set of
    * indices of the DFA states in which that NFA state is a member.
    */
  protected def assembleDFA(
    dfaStates: IndexedSeq[Set[S]],
    initialStateIdx: Int,
    dfaFinals: Set[Int],
    transitionsSeq: IndexedSeq[T],
    dfaTransitions: Array[Array[Int]],
    tracker: IndexSetsTracker,
    appearsIn: Array[Set[Int]]
  ): D[G[S], T]

  override def toString():String = {
    val bld:StringBuilder = new StringBuilder
    for (st <- states) {
      if (isInitialState(st)) bld ++= "> " else bld ++= "  "
      bld ++= st.toString() + "\n"
      for (tr <- labels)
        bld ++= ("  - " + tr + " --> " + transitions(st, tr) + "\n")
    }
    bld.toString()
  }

  override protected def dumpHeader(out: java.io.PrintStream = Console.out):
      Unit =
    out.println("---------- NDFA dump")
}
