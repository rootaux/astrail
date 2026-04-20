package io.joern.dataflowengineoss

import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, FullNameSemantics}
import io.joern.dataflowengineoss.semanticsloader.FlowPath.{FlowMapping, PassThroughMapping}
import io.shiftleft.codepropertygraph.generated.Operators

import scala.annotation.unused

object DefaultSemantics {

  /** @return
    *   a default set of common external procedure calls for all languages.
    */
  def apply(): FullNameSemantics = {
    val list = operatorFlows ++ cFlows ++ javaCollectionFlows ++ javaFlows
    FullNameSemantics.fromList(list)
  }

  private def F = (x: String, y: List[(Int, Int)]) => FlowSemantic.from(x, y)

  private def PTF(x: String, ys: List[(Int, Int)] = List.empty): FlowSemantic =
    FlowSemantic(x).copy(mappings = FlowSemantic.from(x, ys).mappings :+ PassThroughMapping)

  def operatorFlows: List[FlowSemantic] = List(
    F(Operators.addition, List((1, -1), (2, -1))),
    F(Operators.addressOf, List((1, -1))),
    F(Operators.assignment, List((2, 1), (2, -1))),
    F(Operators.assignmentAnd, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentArithmeticShiftRight, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentDivision, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentExponentiation, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentLogicalShiftRight, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentMinus, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentModulo, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentMultiplication, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentOr, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentPlus, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentShiftLeft, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentXor, List((2, 1), (1, 1), (2, -1))),
    F(Operators.cast, List((1, -1), (2, -1))),
    F(Operators.computedMemberAccess, List((1, -1))),
    F(Operators.conditional, List((2, -1), (3, -1))),
    F(Operators.elvis, List((1, -1), (2, -1))),
    F(Operators.notNullAssert, List((1, -1))),
    F(Operators.fieldAccess, List((1, -1))),
    F(Operators.getElementPtr, List((1, -1))),
    PTF(Operators.modulo, List.empty),
    PTF(Operators.arrayInitializer, List.empty),

    // TODO does this still exist?
    F("<operator>.incBy", List((1, 1), (2, 1), (3, 1), (4, 1))),
    F(Operators.indexAccess, List((1, -1))),
    F(Operators.indirectComputedMemberAccess, List((1, -1))),
    F(Operators.indirectFieldAccess, List((1, -1))),
    F(Operators.indirectIndexAccess, List((1, -1), (2, 1))),
    F(Operators.indirectMemberAccess, List((1, -1))),
    F(Operators.indirection, List((1, -1))),
    F(Operators.memberAccess, List((1, -1))),
    F(Operators.pointerShift, List((1, -1))),
    F(Operators.postDecrement, List((1, 1), (1, -1))),
    F(Operators.postIncrement, List((1, 1), (1, -1))),
    F(Operators.preDecrement, List((1, 1), (1, -1))),
    F(Operators.preIncrement, List((1, 1), (1, -1))),
    F(Operators.sizeOf, List.empty[(Int, Int)]),

    // Language specific operators
    PTF("<operator>.tupleLiteral"),
    PTF("<operator>.dictLiteral"),
    PTF("<operator>.setLiteral"),
    PTF("<operator>.listLiteral")
  )

  /** Semantic summaries for common external C/C++ calls.
    *
    * @see
    *   <a href="https://www.ibm.com/docs/en/i/7.3?topic=extensions-standard-c-library-functions-table-by-name">Standard
    *   C Library Functions</a>
    */
  def cFlows: List[FlowSemantic] = List(
    F("abs", List((1, 1), (1, -1))),
    F("abort", List.empty[(Int, Int)]),
    F("asctime", List((1, 1), (1, -1))),
    F("asctime_r", List((1, 1), (1, -1))),
    F("atof", List((1, 1), (1, -1))),
    F("atoi", List((1, 1), (1, -1))),
    F("atol", List((1, 1), (1, -1))),
    F("calloc", List((1, -1), (2, -1))),
    F("ceil", List((1, 1), (1, 1))),
    F("clock", List.empty[(Int, Int)]),
    F("ctime", List((1, -1))),
    F("ctime64", List((1, -1))),
    F("ctime_r", List((1, -1))),
    F("ctime64_r", List((1, -1))),
    F("difftime", List((1, -1), (2, -1))),
    F("difftime64", List((1, -1), (2, -1))),
    PTF("div"),
    F("exit", List((1, 1))),
    F("exp", List((1, -1))),
    F("fabs", List((1, -1))),
    F("fclose", List((1, 1), (1, -1))),
    F("fdopen", List((1, -1), (2, -1))),
    F("feof", List((1, 1), (1, -1))),
    F("ferror", List((1, 1), (1, -1))),
    F("fflush", List((1, 1), (1, -1))),
    F("fgetc", List((1, 1), (1, -1))),
    F("fwrite", List((1, 1), (1, -1), (2, -1), (3, -1), (4, -1))),
    F("free", List((1, 1))),
    F("getc", List((1, 1))),
    F("scanf", List((2, 2))),
    F("strcmp", List((1, 1), (1, -1), (2, 2), (2, -1))),
    F("strlen", List((1, 1), (1, -1))),
    F("strncpy", List((1, 1), (2, 2), (3, 3), (1, -1), (2, -1))),
    F("strncat", List((1, 1), (2, 2), (3, 3), (1, -1), (2, -1)))
  )

  /** Semantic summaries for Java collection types (key-insensitive). Uses `regex = true` to match
    * regardless of resolved/unresolved signatures.
    */
  def javaCollectionFlows: List[FlowSemantic] = {
    // -- Map --
    val mapFlows = List(
      FlowSemantic("java.util..*\\.put:.*", List(FlowMapping(2, 0), FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.putIfAbsent:.*", List(FlowMapping(2, 0), FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.putAll:.*", List(FlowMapping(1, 0)), regex = true),
      FlowSemantic("java.util..*\\.get:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.getOrDefault:.*", List(FlowMapping(0, -1), FlowMapping(2, -1)), regex = true),
      FlowSemantic("java.util..*\\.remove:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.values:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.keySet:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.entrySet:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.compute:.*", List(FlowMapping(0, -1), FlowMapping(2, 0)), regex = true),
      FlowSemantic("java.util..*\\.computeIfAbsent:.*", List(FlowMapping(0, -1), FlowMapping(2, 0)), regex = true),
      FlowSemantic("java.util..*\\.computeIfPresent:.*", List(FlowMapping(0, -1), FlowMapping(2, 0)), regex = true),
      FlowSemantic("java.util..*\\.merge:.*", List(FlowMapping(0, -1), FlowMapping(2, 0)), regex = true),
      FlowSemantic("java.util..*\\.replace:.*", List(FlowMapping(2, 0), FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.Map\\$Entry\\.getValue:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.Map\\$Entry\\.getKey:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.Map\\$Entry\\.setValue:.*", List(FlowMapping(1, 0), FlowMapping(0, -1)), regex = true)
    )

    // -- List --
    val listFlows = List(
      FlowSemantic("java.util..*\\.add:.*", List(FlowMapping(1, 0)), regex = true),
      FlowSemantic("java.util..*\\.addAll:.*", List(FlowMapping(1, 0)), regex = true),
      FlowSemantic("java.util..*\\.get:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.set:.*", List(FlowMapping(2, 0), FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.subList:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.toArray:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.indexOf:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.contains:.*", List(FlowMapping(0, -1), FlowMapping(1, -1)), regex = true),
      FlowSemantic("java.util..*\\.sort:.*", List(FlowMapping(0, 0)), regex = true),
      FlowSemantic("java.util.Collections\\.unmodifiable.*", List(FlowMapping(1, -1)), regex = true),
      FlowSemantic("java.util.Collections\\.synchronized.*", List(FlowMapping(1, -1)), regex = true),
      FlowSemantic("java.util.Collections\\.singleton.*", List(FlowMapping(1, -1)), regex = true),
      FlowSemantic("java.util.Collections\\.empty.*", List(), regex = true),
      FlowSemantic("java.util.List\\.of:.*", List(PassThroughMapping), regex = true),
      FlowSemantic("java.util.List\\.copyOf:.*", List(FlowMapping(1, -1)), regex = true),
      FlowSemantic("java.util.Arrays\\.asList:.*", List(PassThroughMapping), regex = true)
    )

    // -- Queue / Deque --
    val queueFlows = List(
      FlowSemantic("java.util..*\\.offer:.*", List(FlowMapping(1, 0)), regex = true),
      FlowSemantic("java.util..*\\.push:.*", List(FlowMapping(1, 0)), regex = true),
      FlowSemantic("java.util..*\\.addFirst:.*", List(FlowMapping(1, 0)), regex = true),
      FlowSemantic("java.util..*\\.addLast:.*", List(FlowMapping(1, 0)), regex = true),
      FlowSemantic("java.util..*\\.offerFirst:.*", List(FlowMapping(1, 0)), regex = true),
      FlowSemantic("java.util..*\\.offerLast:.*", List(FlowMapping(1, 0)), regex = true),
      FlowSemantic("java.util..*\\.poll:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.pop:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.peek:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.element:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.getFirst:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.getLast:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.peekFirst:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.peekLast:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.pollFirst:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.pollLast:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.removeFirst:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.removeLast:.*", List(FlowMapping(0, -1)), regex = true)
    )

    // -- Set --
    val setFlows = List(
      FlowSemantic("java.util.Set\\.of:.*", List(PassThroughMapping), regex = true),
      FlowSemantic("java.util.Set\\.copyOf:.*", List(FlowMapping(1, -1)), regex = true)
    )

    // -- Iterator / Iterable --
    val iteratorFlows = List(
      FlowSemantic("java.util..*\\.iterator:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.listIterator:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.descendingIterator:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.spliterator:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.Iterator\\.next:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.ListIterator\\.previous:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.lang.Iterable\\.forEach:.*", List(FlowMapping(0, 1)), regex = true),
      FlowSemantic("java.util..*\\.forEach:.*", List(FlowMapping(0, 1)), regex = true)
    )

    // -- Stream --
    val streamFlows = List(
      FlowSemantic("java.util..*\\.stream:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util..*\\.parallelStream:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.stream.Stream\\.of:.*", List(PassThroughMapping), regex = true),
      FlowSemantic("java.util.stream.Stream\\.concat:.*", List(FlowMapping(1, -1), FlowMapping(2, -1)), regex = true),
      FlowSemantic("java.util.stream.Stream\\.map:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.stream.Stream\\.flatMap:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.stream.Stream\\.filter:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.stream.Stream\\.peek:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.stream.Stream\\.sorted:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.stream.Stream\\.distinct:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.stream.Stream\\.limit:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.stream.Stream\\.skip:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.stream.Stream\\.collect:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.stream.Stream\\.toList:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.stream.Stream\\.toArray:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.stream.Stream\\.findFirst:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.stream.Stream\\.findAny:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.stream.Stream\\.reduce:.*", List(FlowMapping(0, -1), PassThroughMapping), regex = true),
      FlowSemantic("java.util.stream.Stream\\.min:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.stream.Stream\\.max:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.stream.Stream\\.forEach:.*", List(FlowMapping(0, 1)), regex = true),
      FlowSemantic("java.util.stream.Stream\\.forEachOrdered:.*", List(FlowMapping(0, 1)), regex = true),
      FlowSemantic("java.util.stream.Collectors\\.toList:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.stream.Collectors\\.toSet:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.stream.Collectors\\.toMap:.*", List(FlowMapping(0, -1), PassThroughMapping), regex = true),
      FlowSemantic(
        "java.util.stream.Collectors\\.toUnmodifiableList:.*",
        List(FlowMapping(0, -1)),
        regex = true
      ),
      FlowSemantic("java.util.stream.Collectors\\.joining:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic(
        "java.util.stream.Collectors\\.groupingBy:.*",
        List(FlowMapping(0, -1), PassThroughMapping),
        regex = true
      )
    )

    // -- Optional --
    val optionalFlows = List(
      FlowSemantic("java.util.Optional\\.of:.*", List(FlowMapping(1, -1)), regex = true),
      FlowSemantic("java.util.Optional\\.ofNullable:.*", List(FlowMapping(1, -1)), regex = true),
      FlowSemantic("java.util.Optional\\.get:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.Optional\\.orElse:.*", List(FlowMapping(0, -1), FlowMapping(1, -1)), regex = true),
      FlowSemantic("java.util.Optional\\.orElseGet:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.Optional\\.orElseThrow:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.Optional\\.map:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.Optional\\.flatMap:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.Optional\\.filter:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.util.Optional\\.ifPresent:.*", List(FlowMapping(0, 1)), regex = true),
      FlowSemantic("java.util.Optional\\.stream:.*", List(FlowMapping(0, -1)), regex = true)
    )

    // -- StringBuilder / StringBuffer --
    val stringBuilderFlows = List(
      FlowSemantic("java.lang.StringBuilder\\.append:.*", List(FlowMapping(1, 0), FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.lang.StringBuffer\\.append:.*", List(FlowMapping(1, 0), FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.lang.StringBuilder\\.insert:.*", List(FlowMapping(2, 0), FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.lang.StringBuffer\\.insert:.*", List(FlowMapping(2, 0), FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.lang.StringBuilder\\.toString:.*", List(FlowMapping(0, -1)), regex = true),
      FlowSemantic("java.lang.StringBuffer\\.toString:.*", List(FlowMapping(0, -1)), regex = true)
    )

    mapFlows ++ listFlows ++ queueFlows ++ setFlows ++ iteratorFlows ++ streamFlows ++ optionalFlows ++ stringBuilderFlows
  }

  /** Semantic summaries for common external Java calls. */
  def javaFlows: List[FlowSemantic] = List(
    PTF("java.lang.String.split:java.lang.String[](java.lang.String)", List((0, 0))),
    PTF("java.lang.String.split:java.lang.String[](java.lang.String,int)", List((0, 0))),
    PTF("java.lang.String.compareTo:int(java.lang.String)", List((0, 0))),
    F("java.io.PrintWriter.print:void(java.lang.String)", List((0, 0), (1, 1))),
    F("java.io.PrintWriter.println:void(java.lang.String)", List((0, 0), (1, 1))),
    F("java.io.PrintStream.println:void(java.lang.String)", List((0, 0), (1, 1))),
    PTF("java.io.PrintStream.print:void(java.lang.String)", List((0, 0))),
    F("android.text.TextUtils.isEmpty:boolean(java.lang.String)", List((0, -1), (1, -1))),
    F("java.sql.PreparedStatement.prepareStatement:java.sql.PreparedStatement(java.lang.String)", List((1, -1))),
    F("java.sql.PreparedStatement.prepareStatement:setDouble(int,double)", List((1, 1), (2, 2))),
    F("java.sql.PreparedStatement.prepareStatement:setFloat(int,float)", List((1, 1), (2, 2))),
    F("java.sql.PreparedStatement.prepareStatement:setInt(int,int)", List((1, 1), (2, 2))),
    F("java.sql.PreparedStatement.prepareStatement:setLong(int,long)", List((1, 1), (2, 2))),
    F("java.sql.PreparedStatement.prepareStatement:setShort(int,short)", List((1, 1), (2, 2))),
    F("java.sql.PreparedStatement.prepareStatement:setString(int,java.lang.String)", List((1, 1), (2, 2))),
    F("org.apache.http.HttpRequest.<init>:void(org.apache.http.RequestLine)", List((1, 1), (1, 0))),
    F("org.apache.http.HttpRequest.<init>:void(java.lang.String,java.lang.String)", List((1, 1), (1, 0), (2, 0))),
    F(
      "org.apache.http.HttpRequest.<init>:void(java.lang.String,java.lang.String,org.apache.http.ProtocolVersion)",
      List((1, 1), (1, 0), (2, 2), (2, 0), (3, 3), (3, 0))
    ),
    F("org.apache.http.HttpResponse.getStatusLine:org.apache.http.StatusLine()", List((0, -1))),
    F("org.apache.http.HttpResponse.setStatusLine:void(org.apache.http.StatusLine)", List((1, 0), (1, 1), (0, -1))),
    F("org.apache.http.HttpResponse.setReasonPhrase:void(java.lang.String)", List((1, 0), (1, 1), (0, -1))),
    F("org.apache.http.HttpResponse.getEntity:org.apache.http.HttpEntity()", List((0, -1))),
    F("org.apache.http.HttpResponse.setEntity:void(org.apache.http.HttpEntity)", List((1, 0), (1, 1), (1, 0)))
  )

  /** @return
    *   procedure semantics for operators and common external Java calls only.
    */
  @unused
  def javaSemantics(): FullNameSemantics = FullNameSemantics.fromList(operatorFlows ++ javaCollectionFlows ++ javaFlows)

}
