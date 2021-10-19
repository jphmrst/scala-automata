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
import org.maraist.fa.traits
import org.maraist.fa.styles.{AutomatonStyle, DOT}

/** Partial implementation of an
  * [[traits.FA indexed finite automaton]] using
  * [[scala.collection.immutable.IndexedSeq `IndexedSeq`s]] and
  * `Array`s.
  *
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  * @tparam Z Type of style options for Graphviz export
  *
  * @group DFA
  */
trait UnindexedFA[S, T, -Z[S, T] <: AutomatonStyle[S, T]]

extends traits.UnindexedFA[S, T, Z] {

  protected def checkState: Unit = { }

  override def foreachState(action: (s: S) => Unit): Unit =
    for(s <- states) do action(s)

  override def foreachInitialState(action: (s: S) => Unit): Unit =
    for(s <- initialStates) do action(s)

  override def foreachFinalState(action: (s: S) => Unit): Unit =
    for(s <- finalStates) do action(s)

  override def foreachTransition(action: (s1: S, t: T, s2: S) => Unit): Unit =
    for (s0 <- states; t <- labels; s1 <- transitions(s0, t))
      do action(s0, t, s1)

  override def toDOT(using Z[S, T]):
      String = {
    // println("       In DFA.toDOT with " + options)
    val stateList = IndexedSeq.from(states)
    val labelsList = IndexedSeq.from(labels)
    val sb = new StringBuilder()
    internalsToDOT(stateList, labelsList, sb)
    sb.toString()
  }

  /** Internal routine used by {@link #toDOT}.  Subclesses may override,
   *  but still call super.internalsToDOT, to extend the Graphviz
   *  representation of a DFA.  */
  protected def internalsToDOT(
    stateList: IndexedSeq[S],
    theLabels: IndexedSeq[T],
    sb: StringBuilder
  )(using style: Z[S, T]):
      Unit = {
    initPlot(sb, stateList.size, theLabels.size)
    for(si <- 0 until stateList.length) {
      val s = stateList(si)
      plotState(sb, style, si, s, isInitialState(s), isFinalState(s))
    }
    afterStatePlot(sb, stateList)
    plotTransitions(stateList, theLabels, sb, style)
    finishPlot(sb)
  }

  protected def plotTransitions(
    stateList: IndexedSeq[S],
    theLabels: IndexedSeq[T],
    sb: StringBuilder,
    style: Z[S, T]):
      Unit = {
    foreachTransition((s0, t, s1) =>
      plotPresentEdge(
        sb, style,
        stateList.indexOf(s0), s0,
        theLabels.indexOf(t), t,
        stateList.indexOf(s1), s1))
    foreachETransition((s0, s1) =>
      plotPresentEdge(
        sb, style,
        stateList.indexOf(s0), s0,
        stateList.indexOf(s1), s1))
  }

  protected def initPlot(sb: StringBuilder, states: Int, labels: Int): Unit = {
  }

  protected def finishPlot(sb: StringBuilder): Unit = { }

  protected def plotState(
    sb: StringBuilder,
    style: Z[S, T],
    si: Int, s: S,
    isInitial: Boolean,
    isFinal: Boolean):
      Unit = {
    sb ++= DOT.tabToVmark
    sb ++= Integer.toString(si)
    sb ++= " [shape="
    if (isFinal) {
      sb ++= style.finalNodeShape(s, this)
    } else {
      sb ++= style.nodeShape(s, this)
    }
    sb ++= ",label=<<sup><font color=\"#0000ff\">"
    sb ++= si.toString()
    sb ++= "</font></sup>"
    sb ++= style.nodeLabel(s, this)
    sb ++= ">]\n"
  }

  protected def afterStatePlot(
    sb: StringBuilder,
    stateList: IndexedSeq[S]):
      Unit = {
    // Arrow for the initial state
    foreachInitialState((s) => {
      plotInitialStateMarker(sb, s, stateList.indexOf(s))
    })
    sb ++= ";\n"
  }

  protected def plotInitialStateMarker(sb: StringBuilder, s: S, idx: Int):
      Unit = {

    // Dummy state for arrow base.
    sb ++= "\tinit"
    sb ++= idx.toString
    sb ++= " [shape=none, margin=0, label=\"\"];"
    // sb ++= " // "
    // sb ++= (if s == null then "null" else s.toString())
    // sb ++= "\n"

    // Arrow from the dummy state to the initial state.
    sb ++= "\tinit"
    sb ++= idx.toString
    sb ++= " -> V"
    sb ++= idx.toString
  }

  protected def plotPresentEdge(
    sb: StringBuilder, style: Z[S, T],
    si0: Int, s0:S, ti0: Int, t:T, si1: Int, s1:S):
      Unit = {
    sb ++= DOT.tabToVmark
    sb ++= Integer.toString(si0)
    sb ++= DOT.graphvizArrowToVmark
    sb ++= Integer.toString(si1)
    sb ++= " [ label=<"
    sb ++= edgeText(style, si0, s0, ti0, t, si1, s1)
    sb ++= "> ];\n"
    //println(si0 + "--[" + t + "]-->" + si1);
  }

  protected def edgeText(
    style: Z[S, T], si0: Int, s0:S, ti0: Int, t:T, si1: Int, s1:S
  ): String =
    style.edgeLabel(t, s0, s1, this)

  protected def plotPresentEdge(
    sb: StringBuilder, style: Z[S, T],
    si0: Int, s0:S, si1: Int, s1:S):
      Unit = {
    sb ++= DOT.tabToVmark
    sb ++= Integer.toString(si0)
    sb ++= DOT.graphvizArrowToVmark
    sb ++= Integer.toString(si1)
    val text = edgeText(style, si0, s0, si1, s1)
    if !text.equals("") then {
       sb ++= " [ label=<"
       sb ++= text
       sb ++= "> ]"
    }
    sb ++= ";\n"
    //println(si0 + "--[" + t + "]-->" + si1);
  }

  protected def edgeText(style: Z[S, T], si0: Int, s0:S, si1: Int, s1:S):
      String = ""

  // =================================================================

  def dump: Unit = {
    dumpHeader()
    dumpStates()
    dumpTransitions()
    dumpFooter()
  }

  protected def dumpHeader(): Unit = println("---------- FA dump")
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
    foreachTransition((src, label, dest) => dumpTransition(src, label, dest))
    foreachETransition((src, dest) => dumpTransition(src, dest))
  }

  protected def dumpTransition(src: S, label: T, dest: S): Unit = {
    println("- " + src + " -[ " + label + " ]-> " + dest)
  }

  protected def dumpTransition(src: S, dest: S): Unit = {
    println("- " + src + " --> " + dest)
  }
}
