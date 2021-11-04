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
import java.io.PrintStream
import org.typelevel.paiges.Doc
import org.maraist.fa.util.Paiges.*
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
    for ((s0, t, s1) <- transitionTriples) do action(s0, t, s1)

  override def transitionTriples: Iterable[(S, T, S)] =
    for (s0 <- states; t <- labels; s1 <- transitions(s0, t))
    yield (s0, t, s1)

  // =================================================================
  // Graphviz plots of the automaton

  override def toDOT(using Z[S, T]):
      String = {
    // println("       In DFA.toDOT with " + options)
    val stateList = IndexedSeq.from(states)
    val stateMap = {
      val builder = Map.newBuilder[S, Int]
      for (i <- 0 until stateList.length) do builder += ((stateList(i), i))
      builder.result
    }
    val labelsList = IndexedSeq.from(labels)
    val sb = new StringBuilder()
    internalsToDOT(stateList, stateMap, labelsList, sb)
    sb.toString()
  }

  /** Internal routine used by {@link #toDOT}.  Subclesses may override,
   *  but still call super.internalsToDOT, to extend the Graphviz
   *  representation of a DFA.  */
  protected def internalsToDOT(
    stateList: IndexedSeq[S],
    stateMap: Map[S, Int],
    theLabels: IndexedSeq[T],
    sb: StringBuilder
  )(using style: Z[S, T]):
      Unit = {
    initPlot(sb, stateList.size, theLabels.size)
    for(si <- 0 until stateList.length) {
      val s = stateList(si)
      plotState(sb, style, si, s, isInitialState(s), isFinalState(s))
    }
    afterStatePlot(sb, style, stateList, stateMap)
    plotTransitions(stateList, stateMap, theLabels, sb, style)
    finishPlot(sb)
  }

  protected def plotTransitions(
    stateList: IndexedSeq[S],
    stateMap: Map[S, Int],
    theLabels: IndexedSeq[T],
    sb: StringBuilder,
    style: Z[S, T]):
      Unit = {
    foreachTransition((s0, t, s1) =>
      plotPresentEdge(
        sb, style, stateList, stateMap,
        stateMap(s0), s0,
        theLabels.indexOf(t), t,
        stateMap(s1), s1))
    foreachETransition((s0, s1) =>
      plotPresentEdge(
        sb, style, stateList, stateMap,
        stateMap(s0), s0,
        stateMap(s1), s1))
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
    style: Z[S, T],
    stateList: IndexedSeq[S],
    stateMap: Map[S, Int]):
      Unit = {
    // Arrow for the initial state
    foreachInitialState((s) => {
      plotInitialStateMarker(sb, style, s, stateMap(s))
    })
  }

  /** Add decorations for an initial state. */
  protected def plotInitialStateMarker(
    sb: StringBuilder, style: Z[S, T], s: S, idx: Int):
      Unit = {

    // Dummy state for arrow base.
    sb ++= "\tinit"
    sb ++= idx.toString
    sb ++= " [shape=none, margin=0, label=\"\"];\n"

    // Arrow from the dummy state to the initial state.
    sb ++= "\tinit"
    sb ++= idx.toString
    sb ++= " -> V"
    sb ++= idx.toString
    sb ++= ";\n"
  }

  protected def plotPresentEdge(
    sb: StringBuilder, style: Z[S, T],
    stateList: IndexedSeq[S],
    stateMap: Map[S, Int],
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
    sb: StringBuilder,
    style: Z[S, T],
    stateList: IndexedSeq[S],
    stateMap: Map[S, Int],
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

  def dump(out: PrintStream = Console.out): Unit =
    out.println(pretty.render(79))

  def pretty: Doc = (
    prettyHeader
      / prettyStates
      / prettyTransitions
      / prettyFooter
  )

  protected def prettyHeader: Doc =
    Doc.text("---------- FA dump")
  protected def prettyFooter: Doc =
    Doc.text("----------")

  protected def prettyStates: Doc =
    (for(state <- states) yield (Doc.line + prettyState(state)))
      .foldLeft(Doc.text("States:"))(_ + _)

  protected def prettyState(s: S): Doc =
    prettyStateLeader(s) + toDoc(s.asInstanceOf[Matchable]) :+ {
      val sb = new StringBuilder
      if (isInitialState(s) || isFinalState(s))  sb ++= " ("
      if (isInitialState(s)) sb ++= "initial"
      if (isInitialState(s) && isFinalState(s)) sb ++= ", "
      if (isFinalState(s)) sb ++= "final"
      if (isInitialState(s) || isFinalState(s)) sb ++= ")"
      sb.toString
    }

  protected def prettyStateLeader(s: S): Doc = Doc.text("- ")

  protected def prettyTransitions: Doc =
    Doc.text("Transitions:")
      / (for ((src, label, dest) <- transitionTriples)
         yield prettyTransition(src, label, dest)).stack
      / (for ((src, dest) <- eTransitionPairs)
         yield prettyETransition(src, dest)).stack

  protected def prettyTransition(src: S, label: T, dest: S): Doc =
    assemblePrettyTransition(
      prettyStateInTransition(src),
      prettyTransitionArrow(src, label, dest),
      prettyStateInTransition(dest))

  protected def prettyETransition(src: S, dest: S): Doc =
    assemblePrettyTransition(
      prettyStateInTransition(src),
      prettyETransitionArrow(src, dest),
      prettyStateInTransition(dest))

  protected def assemblePrettyTransition(before: Doc, arrow: Doc, after: Doc):
      Doc = Doc.text("- ") + after.prefixBy(arrow).prefixBy(before)

  protected def prettyTransitionArrow(src: S, label: T, dest: S): Doc =
    Doc.text("    -[ " + label + " ]-> ")

  protected def prettyETransitionArrow(src: S, dest: S): Doc =
    Doc.text("    --> ")

  protected def prettyStateInTransition(state: S): Doc =
    toDoc(state.asInstanceOf[Matchable])
}
