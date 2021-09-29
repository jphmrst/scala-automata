// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa
import scala.collection.mutable.{Builder, HashMap, HashSet}
import org.maraist.graphviz.
  {Graphable, GraphvizOptions, NodeLabeling, TransitionLabeling}
import org.maraist.fa.traits.
  { StateHolder, FinalStateSetHolder,
    InitialStateSetHolder, DeterministicLabelledTransitionHolder,
    IndexedStateHolder, IndexedLabelsHolder, IndexedSingleInitialStateHolder,
    IndexedFinalStateSetHolder, SingleInitialStateHolder}
import org.maraist.fa.impl.
  {SingleInitialStateMixinElement,
    StateBuilderElement, FinalStateSetBuilderElement,
    DeterministicLabelledTransitionMixinElement}
import org.maraist.fa.elements.
  {HasBuilder, HasBuilderWithInit, AddTransition, RemoveTransition}
import org.maraist.fa.DFA.DFAtraverser
import org.maraist.fa.impl.HashDFABuilder

/** Trait of the basic usage operations on a DFA.
 *
 *  @tparam S The type of all states of the automaton
 *  @tparam T The type of labels on transitions of the automaton
 *
 * @group DFA
 */
trait DFA[S,T]
    extends StateHolder[S]
    with FinalStateSetHolder[S]
    with SingleInitialStateHolder[S]
    with DeterministicLabelledTransitionHolder[S, T]
    with Graphable[S,T] {
  type Traverser <: DFAtraverser[S,T]

  /** Check whether the automaton accepts the given sequence */
  def accepts(ts:Seq[T]): Boolean

  override def toString():String = {
    val bld:StringBuilder = new StringBuilder
    for (st <- states) {
      if (isInitialState(st)) bld ++= "> " else bld ++= "  "
      bld ++= st.toString() + "\n"
      for (tr <- labels)
        transition(st, tr) match {
          case Some(x) => bld ++= ("  - " + tr + " --> " + x + "\n")
          case None =>
        }
    }
    bld.toString()
  }

  /** Traverse the structure of this DFA, states first, then transitions */
  def traverse(trav:Traverser) = {
    val stateList = IndexedSeq.from(states)
    val theLabels = IndexedSeq.from(labels)
    trav.init(stateList.size, theLabels.size)
    for(si <- 0 until stateList.length) {
      val s = stateList(si)
      trav.state(si,s,isInitialState(s),isFinalState(s))
    }
    trav.postState()
    for(si0 <- 0 until stateList.length) {
      val s0 = stateList(si0)
      traverseEdgesFrom(si0, s0, stateList, theLabels, trav)
    }
    trav.finish()
  }

  protected def traverseEdgesFrom(si0:Int, s0:S, stateList:IndexedSeq[S],
                                  theLabels:IndexedSeq[T], trav:Traverser) = {
    for(ti <- 0 until theLabels.length) {
      val t = theLabels(ti)
      transition(s0,t) match {
        case Some(s1) =>
          trav.presentEdge(si0, s0, ti, t, stateList.indexOf(s1), s1)
        case None => trav.absentEdge(si0, s0, ti, t)
      }
    }
  }

  protected def dotTraverser(sb:StringBuilder,
                             stateList:IndexedSeq[S]): Traverser

  /** Internal routine used by {@link #toDOT}.  Subclesses should
   *  override, but still call super.internalsToDOT, to extend the
   *  Graphviz representation of a DFA */
  protected def internalsToDOT(stateList:IndexedSeq[S], sb:StringBuilder
  )(using
    nodeLabeling: NodeLabeling[S, T],
    trLabeling: TransitionLabeling[T],
    graphvizOptions: GraphvizOptions):
      Unit = {

    // Initial state
    sb ++= "\tinit [shape=none, margin=0, label=\"\"];\n"

    traverse(dotTraverser(sb, stateList))
    for(si0 <- 0 until stateList.length) {
      val s0 = stateList(si0)
      for(t <- labels) {
        transition(s0,t) match {
          case Some(s1) => {
          }
          case None => { }
        }
      }
    }
  }

  /** {@inheritDoc} */
  override def toDOT
    (using NodeLabeling[S, T], TransitionLabeling[T], GraphvizOptions):
      String = {
    val stateList = IndexedSeq.from(states)
    val sb = new StringBuilder()
    internalsToDOT(stateList,sb)
    sb.toString()
  }

  // ---------------------------------------------------------------
  // Dumping out the DFA as text

  def dump(): Unit = {
    dumpHeader()
    dumpStates()
    dumpTransitions()
    dumpOther()
    dumpFooter()
  }

  protected def dumpHeader(): Unit = println("---------- DFA dump")
  protected def dumpFooter(): Unit = println("----------")

  protected def dumpStates(): Unit = {
    println("States:")
    for(state <- states) {
      dumpState(state)
    }
  }

  protected def dumpState(s: S): Unit = {
    print("- " + s)
    if (isInitialState(s) || isFinalState(s)) print(" (")
    if (isInitialState(s)) print("initial")
    if (isInitialState(s) && isFinalState(s)) print(", ")
    if (isFinalState(s)) print("final")
    if (isInitialState(s) || isFinalState(s)) print(")")
    println()
  }

  protected def dumpTransitions(): Unit = {
    println("Transitions:")
    for(src <- states) {
      for(label <- labels) {
        transition(src, label) match {
          case None => { }
          case Some(dest) => dumpTransition(src, label, dest)
        }
      }
    }
  }

  protected def dumpTransition(src: S, label: T, dest: S): Unit = {
    println("- " + src + " -[ " + label + " ]-> " + dest)
  }

  protected def dumpOther(): Unit = { }
}

/**
  * @group DFA
  *
  * @groupname DFA Forms of DFAs
  * @groupdesc DFA
  * @groupprio DFA 100
  *
  * @groupname builderElements Arguments to [[Builder]]s for [[DFA]]s
  * @groupdesc builderElements
  * @groupprio builderElements 150
  *
  * @groupname builderPattern [[Builder]] pattern method
  * @groupdesc builderPattern
  * @groupprio builderPattern 160
  */
object DFA {

  /**
    * Methods for traversing the structure of a
    * [[org.maraist.fa.DFA DFA]].  Use with the
    * [[org.maraist.fa.DFA#traverse DFA.traverse]] method. By default,
    * all methods are empty.
    *
    *  @tparam S The type of all states of the automaton
    *  @tparam T The type of labels on transitions of the automaton
    *
    * @group DFA
    */
  trait DFAtraverser[-S,-T] {
    /** Called at the beginning of traversal, before any other methods. */
    def init(states:Int, labels:Int): Unit
    /** Called once for each state in the {@link org.maraist.fa.DFA DFA}. */
    def state(index:Int, state:S, isInitial:Boolean, isFinal:Boolean): Unit
    /**
      * Called after the last call to
      * [[org.maraist.fa.DFA.DFAtraverser#state state]], but before any
      * calls to
      * [[org.maraist.fa.DFA.DFAtraverser#presentEdge presentEdge]] or
      * [[org.maraist.fa.DFA.DFAtraverser#absentEdge absentEdge]].
      */
    def postState(): Unit
    /** Called for each transition between states in the DFA. */
    def presentEdge(fromIndex:Int, fromState:S, labelIndex:Int, label:T,
      toIndex:Int, toState:S): Unit
    /** Called for each state/label pair for which there is no target state. */
    def absentEdge(fromIndex:Int, fromState:S, labelIndex:Int, label:T): Unit
    /** Called last among the methods of this trait for any traversal. */
    def finish(): Unit
  }

  /** Trait of the basic usage operations on a DFA.
    *
    *  @tparam S The type of all states of the automaton
    *  @tparam T The type of labels on transitions of the automaton
    *
    * @group DFA
    */
  trait IndexedDFA[S,T]
      extends DFA[S,T]
      with IndexedStateHolder[S]
      with IndexedLabelsHolder[T]
      with IndexedSingleInitialStateHolder[S]
      with IndexedFinalStateSetHolder[S] {

    def transitionIndex(si:Int, ti:Int): Option[Int]

    /** Traverse the structure of this DFA, states first, then
      * transitions.
      */
    override def traverse(trav:Traverser) = {
      trav.init(states.size, labels.size)
      for(si <- 0 until size) {
        val s = state(si)
        trav.state(si,s,si==initialStateIndex,isFinalState(s))
      }
      trav.postState()
      for(si0 <- 0 until size) {
        val s0 = state(si0)
        traverseEdgesFrom(si0, s0, states, labels, trav)
      }
      trav.finish()
    }

    override protected def traverseEdgesFrom(
      si0:Int, s0:S,
      stateList:IndexedSeq[S],
      theLabels:IndexedSeq[T],
      trav:Traverser):
        Unit = {
      for(ti <- 0 until labels.size) {
        val t = label(ti)
        val toIndex = transitionIndex(si0,ti)
        //println("  traversing " + si0 + "/" + ti + " --> " + toIndex)
        toIndex match {
          case Some(si1) => {
            //println("        echo " + si0 + " " + ti + " " + si1)
            trav.presentEdge(si0, s0, ti, t, si1, state(si1))
          }
          case None => {
            trav.absentEdge(si0, s0, ti, t)
          }
        }
        //println("  end traversing")
      }
    }
  }

  // Directives for the Builder pattern

  /** All [[Builder]]-pattern elements pertaining to [[DFA]]s.
    *
    *  @tparam S The type of all states of the automaton
    *  @tparam T The type of labels on transitions of the automaton
    *
    * @group builderElements
    */
  type DFAelements[S, T] = (
    SingleInitialStateMixinElement[S,T]
      | StateBuilderElement[S,T]
      | FinalStateSetBuilderElement[S,T]
      | DeterministicLabelledTransitionMixinElement[S, T]
  )

  // Fetch a builder for the pattern.

  /** [[Builder]] pattern method for [[DFA]]s.
    *
    *  @tparam S The type of all states of the automaton
    *  @tparam T The type of labels on transitions of the automaton
    *
    * @group builderPattern
    */
  def newBuilder[S, T](initialState: S) =
    newBuilderFor[S, T, HashDFABuilder, DFA](initialState)

  /** [[Builder]] pattern method for [[DFA]]s.
    *
    *  @tparam S The type of all states of the automaton
    *  @tparam T The type of labels on transitions of the automaton
    *
    * @group builderPattern
    */
  def newBuilderFor[
    S, T,
    Bldr[X,Y] <: Builder[DFAelements[X,Y],Impl[X, Y]],
    Impl[X, Y] <: DFA[X,Y]
  ](initialState: S)(using impl: HasBuilderWithInit[DFAelements, Bldr, Impl]) =
    impl.build[S,T](initialState)

  /** Implementation of [[Builder]] pattern for [[DFA]]s.
    *
    * @group builderPattern
    */
  given hashBd: HasBuilderWithInit[DFAelements, HashDFABuilder, DFA] with {
    override def build[S,T](init: S): HashDFABuilder[S, T] =
        new HashDFABuilder[S, T](init)
  }
}
