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

  /** {@inheritDoc} */
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
    // Initial state
    sb ++= "\tinit [shape=none, margin=0, label=\"\"];\n"

    initPlot(sb, stateList.size, theLabels.size)
    for(si <- 0 until stateList.length) {
      val s = stateList(si)
      plotState(sb, style, si, s, isInitialState(s), isFinalState(s))
    }
    afterStatePlot(sb, stateList)
    for(si0 <- 0 until stateList.length) {
      val s0 = stateList(si0)
      foreachTransition((s0, t, s1) =>
        plotPresentEdge(
          sb, style, si0, s0,
          theLabels.indexOf(t), t,
          stateList.indexOf(s1), s1
        )
      )
    }
    finishPlot(sb)
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
    sb ++= "\tinit -> V"
    foreachInitialState((s) => {
      sb ++= Integer.toString(stateList.indexOf(s))
    })
    sb ++= ";\n"
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
    sb ++= style.edgeLabel(t, s0, s1, this)
    sb ++= "> ];\n"
    //println(si0 + "--[" + t + "]-->" + si1);
  }
}
