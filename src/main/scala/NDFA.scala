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
import scala.collection.mutable.{Builder, HashMap, HashSet, Queue}
import org.maraist.graphviz.{Graphable, NodeLabeling, TransitionLabeling}
import org.maraist.fa.traits.
  {StateHolder, FinalStateSetHolder, InitialStateSetHolder,
    LabelsHolder, NondeterministicLabelledTransitionHolder,
    IndexedNondeterministicLabelledTransitionHolder,
    IndexedStateHolder, IndexedLabelsHolder, IndexedInitialStateSetHolder,
    IndexedFinalStateSetHolder,
    UnlabelledTransitionHolder}
import org.maraist.fa.elements.
  {HasBuilder, AddTransition, RemoveTransition,
    AddETransition, RemoveETransition}
import org.maraist.fa.impl.
  {InitialStateSetTraitElements,
    StateBuilderElement, FinalStateSetBuilderElement}
import org.maraist.fa.DFA.IndexedDFA
import org.maraist.fa.impl.{DOT,HashNDFABuilder}

/** Methods provided by nondeterministic finite automata (NDFAs)
  *
  * Trait specifying methods provided by all NDFAs, and providing
  * default implementations for derivations from the core methods.
  *
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the
  * automaton
  * @group NDFA
  */
trait NDFA[S, T, +ThisDFA <: IndexedDFA[Set[S],T]]
    extends StateHolder[S]
    with FinalStateSetHolder[S]
    with InitialStateSetHolder[S]
    with NondeterministicLabelledTransitionHolder[S, T]
    with UnlabelledTransitionHolder[S]
    with Graphable[S,T] {

  /** Converts this NDFA to a [[org.maraist.fa.DFA DFA]]. */
  def toDFA: ThisDFA

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

  /** Internal routine used by {@link #toDOT}.  Subclesses should
   *  override, but still call super.internalsToDOT, to extend the
   *  Graphviz representation of a NDFA */
  protected def internalsToDOT(
    stateList: IndexedSeq[S],
    sb: StringBuilder,
    nodeLabeling: NodeLabeling[S] = this.nodeLabeling,
    trLabeling: TransitionLabeling[T] = this.transitionLabeling
  ):Unit = {

    // Initial state
    sb ++= "\tinit [shape=none, margin=0, label=\"\"];\n"

    for(si <- 0 until stateList.length) {
      val s = stateList(si)
      sb ++= DOT.tabToVmark
      sb ++= Integer.toString(si)
      sb ++= " [shape="
      if (isFinalState(s)) {
        sb ++= graphvizOptions.finalNodeShape
      } else {
        sb ++= graphvizOptions.nodeShape
      }
      sb ++= ",label=<"
      sb ++= nodeLabeling.getLabel(s)
      sb ++= ">];\n"
    }

    // Initial state
    for(init <- getInitialStates) {
      sb ++= "\tinit -> V"
      sb ++= Integer.toString(stateList.indexOf(init))
      sb ++= ";\n"
    }

    for(si0 <- 0 until stateList.length) {
      val s0 = stateList(si0)
      for(s1 <- eTransitions(s0)) {
        writeArrow(sb, si0, s1, stateList, "&epsilon;")
      }
      for(t <- labels) {
        for(s1 <- transitions(s0,t)) {
          writeArrow(
            sb, si0, s1, stateList, getArrowLabel(t, s0, s1, trLabeling))
        }
      }
    }
  }

  protected def
    getArrowLabel(t: T, s0: S, s1: S, trLabeling: TransitionLabeling[T]):
      String = trLabeling.getLabel(t)

  private def writeArrow(sb:StringBuilder, si0:Int, s1:S,
                         stateList:IndexedSeq[S], label:String):Unit = {
    val si1 = stateList.indexOf(s1)
    sb ++= DOT.tabToVmark
    sb ++= Integer.toString(si0)
    sb ++= DOT.graphvizArrowToVmark
    sb ++= Integer.toString(si1)
    sb ++= " [ label=<"
    sb ++= label
    sb ++= "> ];\n"
  }

  /** {@inheritDoc} */
  def toDOT(nodeLabeling:NodeLabeling[S] = this.nodeLabeling,
            transitionLabeling:TransitionLabeling[T] =
              this.transitionLabeling):String = {
    val stateList = IndexedSeq.from(states)
    val sb = new StringBuilder()
    internalsToDOT(stateList,sb,nodeLabeling,transitionLabeling)
    sb.toString()
  }

  def dump():Unit = {
    dumpHeader()
    dumpStates()
    dumpTransitions()
    dumpFooter()
  }

  protected def dumpHeader(): Unit = println("---------- NDFA dump")
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
        for(dest <- transitions(src, label)) {
          dumpTransition(src, label, dest)
        }
      }
      for(dest <- eTransitions(src)) {
        dumpTransition(src, dest)
      }
    }
  }

  protected def dumpTransition(src: S, label: T, dest: S): Unit = {
    println("- " + src + " -[ " + label + " ]-> " + dest)
  }

  protected def dumpTransition(src: S, dest: S): Unit = {
    println("- " + src + " --> " + dest)
  }
}

object NDFA {
  /** Type signature of an NDFA whose states and transition labels can
    * be referenced by an index number.
    *
    *  @tparam S The type of all states of the automaton
    *  @tparam T The type of labels on transitions of the automaton
    *
    * @group NDFA
    */
  trait IndexedNDFA[S, T, +ThisDFA <: IndexedDFA[Set[S],T]]
      extends NDFA[S,T,ThisDFA]
      with IndexedStateHolder[S]
      with IndexedNondeterministicLabelledTransitionHolder[S, T]
      with IndexedInitialStateSetHolder[S]
      with IndexedFinalStateSetHolder[S]

  def newBuilder[S, T, SetType[_], MapType[_,_]](initialState: S)(
    using impl: HasBuilder[
      NDFA.NDFAelements,
      ?,
      [X,Y] =>> NDFA[X, Y, IndexedDFA[Set[X], Y]]
    ]
  ) = impl.build[S,T]()

  type NDFABuilders[S,T] = (
    AddETransition[S] | RemoveETransition[S]
      | AddTransition[S,T]
      | RemoveTransition[S,T]
  )

  type NDFAelements[S, T] = (
    InitialStateSetTraitElements[S,T]
      | StateBuilderElement[S,T]
      | FinalStateSetBuilderElement[S,T]
      | NDFABuilders[S,T]
  )

  given HasBuilder[NDFAelements, HashNDFABuilder, [X,Y] =>> NDFA[X, Y, IndexedDFA[Set[X], Y]]
  ] with {
    override def build[S,T](): HashNDFABuilder[S, T] =
      new HashNDFABuilder[S, T]
  }
}

