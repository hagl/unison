package org.unisonweb

import org.unisonweb.Term.{Name, Term}
import org.unisonweb.compilation.{CurrentRec, IsNotTail, RecursiveVars}
import org.unisonweb.compilation2.Value.Lambda

object compilation2 {

  type U = Double // unboxed values
  val U0: U = 0.0
  type B = Param // boxed values
  type R = Result

  val K = 2

  // type StackPtr = Int
  class StackPtr(private val top: Int) extends AnyVal {
    def toInt = top
    @inline def u(stackU: Array[U], envIndex: Int): U = stackU(top - envIndex + K - 1)
    @inline def b(stackB: Array[B], envIndex: Int): B = stackB(top - envIndex + K - 1)
    @inline def increment(by: Int) = new StackPtr(top+by)
    @inline def pushU(stackU: Array[U], i: Int, u: U): Unit = stackU(top + i + 1) = u
    @inline def pushB(stackB: Array[B], i: Int, b: B): Unit = stackB(top + i + 1) = b
  }

  abstract class Param {
    def toValue: Value
    def isRef: Boolean = false
  }
  object Param {
    def apply(u: U, b: B): Param = if (b eq null) Value.Num(u) else b
  }

  class Ref(val name: Name, computeValue: () => Value) extends Param {
    lazy val value = computeValue()
    def toValue = value
    override def isRef = true
  }

  abstract class Value extends Param {
    def toValue = this
    def decompile: Term
  }
  object Value {
    def apply(u: U, b: Value): Value = if (b eq null) Num(u) else b

    case class Num(n: U) extends Value {
      def decompile = Term.Num(n)
    }

    case class Lambda(arity: Int, body: Computation, decompile: Term) extends Value {
      def maxBinderDepth = 1024 // todo: actually maintain this during compile
    }
  }

  case object SelfCall extends Throwable { override def fillInStackTrace = this }
  case object TailCall extends Throwable { override def fillInStackTrace = this }


  case class Result(var boxed: Value, var tailCall: Lambda, var argsStart: StackPtr, var stackArgsCount: Int,
                    var x1: U, var x0: U, var x1b: B, var x0b: B)

  import Value.Lambda

  /**
    * Computations take a logical stack as an argument. The stack grows
    * to the right, and it is split into an unboxed and boxed representation.
    * The top K elements of the stack are passed as regular function arguments;
    * elements below that are stored in the arrays `stackU` and `stackB`.
    *
    * More specifically:
    *
    *   - `x0` is the top (unboxed) element of the stack
    *   - `x0b` is the top (boxed) element of the stack
    *   - `x1` is immediately below `x0` in the stack
    *   - `stack(top) is immediately below `x1` in the stack
    *   - `stack(top - 1)` is immediately below `stack(top)`, etc.
    *   - To retrieve the ith element (0-based) of the environment is `stack(top - (i - K) - 1)`
    */
  // todo/note: only values (Num/Lambda) get decompiled; don't need to maintain this for
  abstract class Computation(val decompile: Term) {
    def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U
  }

  /** Put `v` into `r.boxed` */
  @inline private def returnBoxed(r: R, v: Value): U = { r.boxed = v; U0 }

  // todo: think through whether can elide
  @inline private def returnUnboxed(r: R, unboxed: U): U = { r.boxed = null; unboxed }

  @inline private def returnBoth(r: R, x0: U, x0b: B) = { if (x0b ne null) r.boxed = x0b.toValue; x0 }


  case class Self(name: Name) extends Computation(Term.Var(name)) {
    def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U =
      returnBoxed(r, rec)
  }

  // todo: maybe opportunities for more expressive matching by breaking this into sub-cases
  abstract class Return(e: Term) extends Computation(e) {
    def value: Value
  }

  object Return {
    def apply(v: Value, t: Term) = v match {
      case Value.Num(d) => compileNum(d)
      case f@Value.Lambda(_, _, _) => new Return(f.decompile) {
        def value: Value = f

        def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U =
          returnBoxed(r, f)
      }
    }
    def unapply(c: Computation): Option[Value] = {
      c match {
        case r: Return => Some(r.value)
        case _ => None
      }
    }
  }

  def compileNum(n: U): Computation =
    new Return(Term.Num(n)) {
      def value = Value.Num(n)

      def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U =
        returnUnboxed(r, n)
    }

  /**
    * Returns true if the register should be saved to the stack when pushing.
    * `i = 0` is the top register, `i = 1` is below that, etc.
    */
  private def shouldSaveRegister(i: Int, env: Vector[Name], freeVars: Set[Name]): Boolean =
    // if the register has valid data, and the variable is still used, preserve it
    (i < env.length) && freeVars.contains(env(i))


  abstract class Push {
    def push1: P1
    def push2: P2
    abstract class P1 {
      def apply(top: StackPtr, stackU: Array[U], stackB: Array[B], u: U, b: B): Unit
    }
    abstract class P2 {
      def apply(top: StackPtr, stackU: Array[U], stackB: Array[B], u1: U, b1: B, u0: U, b0: B): Unit
    }

  }

  def push(env: Vector[Name], freeVars: Set[Name]): Push = new Push {
    def push1 =
      if (!shouldSaveRegister(1, env, freeVars)) new P1 {
        def apply(top: StackPtr, stackU: Array[U], stackB: Array[B], u: U, b: B): Unit = {
          // top.pushB(stackB, 0, null) // todo: should we null these out?
        }

      }
      else new P1 {
        // todo: does single abstract method conversion box `u`?
        def apply(top: StackPtr, stackU: Array[U], stackB: Array[B], u: U, b: B): Unit = {
          top.pushU(stackU, 0, u)
          top.pushB(stackB, 0, b)
        }
      }
    def push2 = {
      val x1garbage: Boolean = !shouldSaveRegister(1, env, freeVars)
      val x0garbage: Boolean = !shouldSaveRegister(0, env, freeVars)

      if (x1garbage && x0garbage) new P2 {
        def apply(top: StackPtr, stackU: Array[U], stackB: Array[B], u1: U, b1: B, u0: U, b0: B): Unit = {
          // top.pushB(stackB, 0, null) // todo: should we null these out?
          // top.pushB(stackB, 1, null)
        }
      }
      else if (x0garbage) new P2 {
        def apply(top: StackPtr, stackU: Array[U], stackB: Array[B], u1: U, b1: B, u0: U, b0: B): Unit = {
          top.pushU(stackU, 0, u1)
          top.pushB(stackB, 0, b1)
          // top.pushB(stackB, 1, null)
        }
      }
      else if (x1garbage) new P2 {
        def apply(top: StackPtr, stackU: Array[U], stackB: Array[B], u1: U, b1: B, u0: U, b0: B): Unit = {
          top.pushU(stackU, 1, u0)
//          top.pushB(stackB, 0, null)
          top.pushB(stackB, 1, b0)
        }
      }
      else new P2 {
        def apply(top: StackPtr, stackU: Array[U], stackB: Array[B], u1: U, b1: B, u0: U, b0: B): Unit = {
          top.pushU(stackU, 0, u1)
          top.pushB(stackB, 0, b1)
          top.pushU(stackU, 1, u0)
          top.pushB(stackB, 1, b0)
        }
      }
    }
  }

  def compileStaticFullySaturatedNontailCall(e: Term, lam: Lambda, body: Computation, compiledArgs: List[Computation]): Computation =
    compiledArgs match {
      case List(arg) => new Computation(e) {
        def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U = {
          val argv = eval(arg, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          val argvb = r.boxed
          body(r, lam, top, stackU, U0, argv, stackB, null, argvb)
        }
      }
      case List(arg1, arg2) => new Computation(e) {
        def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U = {
          val argv1 = eval(arg1, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          val argv1b = r.boxed
          val argv2 = eval(arg2, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          val argv2b = r.boxed
          body(r, lam, top, stackU, argv1, argv2, stackB, argv1b, argv2b)
        }
      }
      case args =>
        new Computation(e) {
          private val argsArray = args.toArray
          def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U = {
            doFullySaturatedCall(lam, argsArray, r, rec, top,
                                 stackU, x1, x0,
                                 stackB, x1b, x0b)
        }
      }
    }

  def compileStaticFullySaturatedTailCall(e: Term, lam: Lambda, compiledArgs: List[Computation]): Computation =
    compiledArgs match {
      case List(arg) => new Computation(e) {
        def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U = {
          r.x0 = eval(arg, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          r.x0b = r.boxed
          r.stackArgsCount = 0
          r.tailCall = lam
          throw TailCall
        }
      }
      case List(arg1, arg2) => new Computation(e) {
        def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U = {
          r.x1 = eval(arg1, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          r.x1b = r.boxed
          r.x0 = eval(arg2, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          r.x0b = r.boxed
          r.stackArgsCount = 0
          r.tailCall = lam
          throw TailCall
        }
      }
      case args =>
        new Computation(e) {
          val argsArray = args.toArray
          def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U = {

            doTailCall(lam, argsArray, r, rec, top,
                       stackU, x1, x0, stackB, x1b, x0b)
          }
        }
    }

  def compileFullySaturatedSelfTailCall(e: Term, compiledArgs: List[Computation]): Computation =
    compiledArgs match {
      case List(arg) => new Computation(e) {
        def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U = {
          r.x0 = eval(arg, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          r.x0b = r.boxed
          throw SelfCall
        }
      }
      case List(arg1, arg2) => new Computation(e) {
        def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U = {
          r.x1 = eval(arg1, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          r.x1b = r.boxed
          r.x0 = eval(arg2, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          r.x0b = r.boxed
          throw SelfCall
        }
      }
      case args => new Computation(e) {
        private val argsArray = args.toArray
        def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U = {
          @annotation.tailrec def go(offset: Int): Unit =
            if (offset < argsArray.length - K) {
              top.pushU(stackU, offset, eval(argsArray(offset), r, rec, top, stackU, x1, x0, stackB, x1b, x0b))
              top.pushB(stackB, offset, r.boxed)
              go(offset + 1)
            }
          go(0)

          r.x1 = eval(argsArray(argsArray.length-2), r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          r.x1b = r.boxed
          r.x0 = eval(argsArray(argsArray.length-1), r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          r.x0b = r.boxed
          throw SelfCall
        }
      }
    }

  def compileFullySaturatedSelfNontailCall(e: Term, compiledArgs: List[Computation]): Computation =
    compiledArgs match {
      case List(arg) => new Computation(e) {
        def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U = {
          val argv = eval(arg, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          val argvb = r.boxed
          rec.body(r, rec, top, stackU, U0, argv, stackB, null, argvb)
        }
      }
      case List(arg1, arg2) => new Computation(e) {
        def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U = {
          val argv1 = eval(arg1, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          val argv1b = r.boxed
          val argv2 = eval(arg2, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          val argv2b = r.boxed
          rec.body(r, rec, top, stackU, argv1, argv2, stackB, argv1b, argv2b)
        }
      }
      case args =>
        new Computation(e) {
          private val argsArray = args.toArray
          def apply(r: R, rec: Lambda, top: StackPtr,
                    stackU: Array[U], x1: U, x0: U,
                    stackB: Array[B], x1b: B, x0b: B): U = {
            doFullySaturatedCall(rec, argsArray, r, rec, top,
                                 stackU, x1, x0, stackB, x1b, x0b)
          }
        }
    }

  def compileUnderappliedCall(builtins: Name => Computation)(
    e: Term, lam: Lambda, compiledArgs: Array[Computation]): Computation = {

    val Term.Lam(names, body) = lam.decompile
    val argCount = compiledArgs.length
    val remNames = names drop argCount
    assert(argCount < lam.arity)
    new Computation(e) {
      def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U = {
        @annotation.tailrec def go(i: Int, substs: Map[Name, Term]): Map[Name, Term] = {
          if (i < argCount) {
            val param =
                Term.Compiled2(Param(eval(compiledArgs(i), r, rec, top, stackU, x1, x0, stackB, x1b, x0b), r.boxed))

            go(i+1, substs.updated(names(i), param))
          }
          else substs
        }
        // Should do no substitution: (\y y y -> y) 0 0
        // because the third y shadows the first two.
        val substs = go(0, Map.empty) -- remNames
        val body2 = ABT.substs(substs)(body)
        val lam2 = Term.Lam(remNames: _*)(body2)
        r.boxed = compile(builtins)(lam2, Vector.empty, CurrentRec.none, RecursiveVars.empty, IsNotTail) match {
          case Return(v) => v
          case v => sys.error("a partially applied lambda should produce another lambda, instead got: " + v)
        }
        U0
      }
    }
  }

  @inline def doTailCall(
    fn: Lambda,
    args: Array[Computation],
    r: R, rec: Lambda, top: StackPtr,
    stackU: Array[U], x1: U, x0: U,
    stackB: Array[B], x1b: B, x0b: B): Nothing = {

    assert(K == 2) // rewrite all the cases if K is different.

    @annotation.tailrec def go(offset: Int): Unit =
      if (offset < args.length - K) {
        top.pushU(stackU, offset, eval(args(offset), r, rec, top, stackU, x1, x0, stackB, x1b, x0b))
        top.pushB(stackB, offset, r.boxed)
        go(offset + 1)
      }
    go(0)

    r.x1 = eval(args(args.length-2), r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
    r.x1b = r.boxed
    r.x0 = eval(args(args.length-1), r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
    r.x0b = r.boxed
    r.argsStart = top.increment(1)
    r.stackArgsCount = args.length - K
    r.tailCall = fn
    throw TailCall
  }

  @inline def doFullySaturatedCall(
    fn: Lambda,
    args: Array[Computation],
    r: R, rec: Lambda, top: StackPtr,
    stackU: Array[U], x1: U, x0: U,
    stackB: Array[B], x1b: B, x0b: B): U = {

    assert(K == 2) // rewrite all the cases if K is different.

    @annotation.tailrec def go(offset: Int): Unit =
      if (offset < args.length - K) {
        top.pushU(stackU, offset, eval(args(offset), r, rec, top, stackU, x1, x0, stackB, x1b, x0b))
        top.pushB(stackB, offset, r.boxed)
        go(offset + 1)
      }
    go(0)

    val argv1 = eval(args(args.length-2), r, rec, top, stackU,
                     x1, x0, stackB, x1b, x0b)
    val argv1b = r.boxed
    val argv2 = eval(args(args.length-1), r, rec, top, stackU,
                     x1, x0, stackB, x1b, x0b)
    val argv2b = r.boxed
    fn.body(r, fn, top.increment(args.length - K), stackU,
            argv1, argv2, stackB, argv1b, argv2b)
  }

  // in `f x`
  // - evaluate `f`
  // - notice `f` has arity 1, and that the call is in tail position
  //    - issue tail call to `f`
  // - if `f` has arity > 1
  def dynamicCall(builtins: Name => Computation)(
                  e: Term, fn: Computation,
                  args: List[Computation], isTail: Boolean): Computation =
    new Computation(e) {
      val argsArray = args.toArray
      val argc = argsArray.size
      def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U = {
        @annotation.tailrec
        def invokeDynamic(fn: Lambda, args: Array[Computation]): U = {
          if (argc == fn.arity)
            if (isTail)
              doTailCall(fn, args, r, rec, top,
                         stackU, x1, x0, stackB, x1b, x0b)
            else doFullySaturatedCall(
                   fn, args, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          else if (argc < fn.arity) {
            val c = compileUnderappliedCall(builtins)(null, fn, args)
            c(r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          }
          else {
            val lam = {
              doFullySaturatedCall(
                fn, args.take(fn.arity), r, rec, top,
                stackU, x1, x0, stackB, x1b, x0b)
              r.boxed.asInstanceOf[Lambda]
            }
            invokeDynamic(lam, args.drop(fn.arity))
          }
        }

        val evalFn = {
          eval(fn, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          r.boxed
        }.asInstanceOf[Lambda]

        invokeDynamic(evalFn, argsArray)
      }
    }

  def compile(builtins: Name => Computation)(
    e: Term, env: Vector[Name], currentRec: CurrentRec, recVars: RecursiveVars,
    isTail: Boolean): Computation = {

    e match {
      case Term.Num(n) => compileNum(n)
      case Term.Builtin(name) => builtins(name)
      case Term.Compiled2(param) => Return(param.toValue, e)
      case Term.Self(name) => new Self(name)
      case Term.Var(name) => compileVar(e, name, env, currentRec)
      case Term.Let1(name, b, body) =>
        val cb = compile(builtins)(b, env, currentRec, recVars, isTail = false)
        val cbody = compile(builtins)(body, name +: env, currentRec.shadow(name),
          recVars - name, isTail)
        val push = compilation2.push(env, Term.freeVars(body)).push1
        new Computation(e) {
          def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U = {
            val rb = eval(cb, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
            val rbb = r.boxed
            push(top, stackU, stackB, rb, rbb)
            cbody(r, rec, top.increment(1), stackU, x1, x0, stackB, x1b, x0b)
          }
        }
      // todo: Let2, etc.

      case Term.Lam(names, body) =>
        val freeVars = Term.freeVars(e)
        // The lambda is closed
        if (freeVars.isEmpty) {
          val cbody = compile(builtins)(body, names.reverse.toVector,
            CurrentRec.none, RecursiveVars.empty, isTail = true)
          Return(Lambda(names.length, cbody, e), e)
        }
        else {
          val compiledFrees: Map[Name, Computation] =
            (freeVars -- recVars.get).view.map {
              name => (name, compileVar(Term.Var(name), name, env, currentRec))
            }.toMap
          val compiledFreeRecs: Map[Name, ParamLookup] =
            freeVars.intersect(recVars.get).view.map {
              name => (name, compileRef(Term.Var(name), name, env, currentRec))
            }.toMap
          new Computation(e) {

            def apply(r: R, rec: Lambda, top: StackPtr,
                      stackU: Array[U], x1: U, x0: U,
                      stackB: Array[B], x1b: B, x0b: B): U = {
              val evaledFreeVars: Map[Name, Term] = compiledFrees.mapValues {
                c =>
                  val evaluatedVar = c(r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
                  val value = Value(evaluatedVar, r.boxed)
                  Term.Compiled2(value)
              }

              val evaledRecVars: Map[Name, Term] = compiledFreeRecs.transform {
                (name, lookup) =>
                  if (currentRec.contains(name)) Term.Self(name)
                  else {
                    val evaluatedVar = lookup(rec, top, stackB, x1b, x0b)
                    if (evaluatedVar eq null) sys.error(name + " refers to null stack slot.")
                    require(evaluatedVar.isRef)
                    Term.Compiled2(evaluatedVar)
                  }
              }

              val lam2 = Term.Lam(names: _*)(
                body = ABT.substs(evaledFreeVars ++ evaledRecVars)(body)
              )
              assert(Term.freeVars(lam2).isEmpty)
              r.boxed = compile(builtins)(
                lam2, Vector(), CurrentRec.none, RecursiveVars.empty, false
              ) match {
                case v: Return => v.value
                case _ => sys.error("compiling a lambda with no free vars should always produce a Return")
              }
              U0
            }
          }
        }

      case Term.Apply(Term.Apply(fn, args), args2) => // converts nested applies to a single apply
        compile(builtins)(Term.Apply(fn, (args ++ args2): _*), env, currentRec, recVars, isTail)

      case Term.Apply(fn, Nil) => sys.error("the parser isn't supposed to produce this")

      case Term.Apply(fn, args) =>
        val compiledArgs: List[Computation] =
          args.view.map(arg => compile(builtins)(arg, env, currentRec, recVars, IsNotTail)).toList

        val cfn: Computation = compile(builtins)(fn, env, currentRec, recVars, IsNotTail)

        cfn match {

          case Return(lam@Lambda(arity, body, _)) =>
            if (args.length == arity) {
              // static tail call, fully saturated
              //   (x -> x+4) 42
              //   ^^^^^^^^^^^^^
              if (isTail)
                compileStaticFullySaturatedTailCall(e, lam, compiledArgs)

              // static non-tail call, fully saturated
              //   ex: let x = (x -> x + 1) 1; x
              //               ^^^^^^^^^^^^^^
              else
                compileStaticFullySaturatedNontailCall(e, lam, body, compiledArgs)
            }
            // static call, underapplied (no tail call variant - just returning immediately)
            //   ex: (x y -> x) 42
            //       ^^^^^^^^^^^^^
            else if (args.length < arity)
              compileUnderappliedCall(builtins)(e, lam, compiledArgs.toArray)

            // static call, overapplied
            //   ex: (x -> x) (x -> x) (x -> x) 42
            //       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            else {
              val fn2: Computation = compileStaticFullySaturatedNontailCall(e, lam, body, compiledArgs.take(lam.arity))
              dynamicCall(builtins)(e, fn2, compiledArgs.drop(lam.arity), isTail)
            }

          case Self(name) if currentRec.contains(name, args.length) =>
            if (isTail)
              compileFullySaturatedSelfTailCall(e, compiledArgs)
            else
              // self non-tail call, fully saturated
              //   ex: let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)
              //                                            ^^^^^^^^^^^   ^^^^^^^^^^^
              compileFullySaturatedSelfNontailCall(e, compiledArgs)

          // dynamic call, also catches self calls, underapplied (either in tail or non-tail position)
          //   ex: let apply f x = f x; ...
          //                       ^^^^
          case _ => dynamicCall(builtins)(e, cfn, compiledArgs, isTail)
        }

      case Term.LetRec(List((name, binding)), body) => ???
      /*
        // 1. compile the body
        // 2. compile all of the bindings into an Array[Computation]
        // 3. construct the Computation

        val cbinding = binding match {
          case l@Term.Lam(argNames, bindingBody) =>
            val newCurrentRec = CurrentRec(name, argNames.size).shadow(argNames)
            if (hasTailRecursiveCall(newCurrentRec, bindingBody))
            // construct the wrapper lambda to catch the SelfTailCalls
              ???
            else // compile the lambda as Normal
              compile(builtins)(l, name +: env, newCurrentRec, recVars + name, IsNotTail)
          case b => // not a lambda, not recursive, could have been a Let1
            compile(builtins)(Term.Let1(name, binding)(body), env, currentRec, recVars, IsNotTail)
        }

        val cbody = compile(builtins)(body, name +: env, currentRec.shadow(name), recVars + name, isTail)

        val push1 = compilation2.push(env, Term.freeVars(body) ++ Term.freeVars(binding)).push1

        // this
        new Computation(e) {

          def apply(r: R, rec: Lambda, top: StackPtr,
                    stackU: Array[U], x1: U, x0: U,
                    stackB: Array[B], x1b: B, x0b: B): U = {

//            lazy val evaledBinding: Ref =
            // eval(cbinding, rec, U0,            x0, x1, x2, pushU(stackU, x3),
            //                     evaledBinding, x0b, x1b, x2b, pushB(stackB, x3b))
            // this might be the general case; for let rec 1, probably can do something simpler?
            // todo: later, can we avoid putting this onto the stack at all? maybe by editing environment
//              new Ref(name, () => Value(eval(cbinding, r, rec, top.increment(1), stackU, x1, x0, stackB, x1b, x0b), r.boxed))

            //            cbody(r, rec, top ?, stackU,
            //
            //              U0, x0, x1, x2, pushU(stackU, x3),
            //              evaledBinding, x0b, x1b, x2b, pushB(stackB, x3b), r)
            ???
          }
        }


//          override def apply(rec: Lambda, x0: U, x1: U, x2: U, x3: U, stackU: Array[U],
//                             x0b: B, x1b: B, x2b: B, x3b: B, stackB: Array[B], r: R): U = {
//            lazy val evaledBinding: Ref =
//            // eval(cbinding, rec, U0,            x0, x1, x2, pushU(stackU, x3),
//            //                     evaledBinding, x0b, x1b, x2b, pushB(stackB, x3b))
//            // this might be the general case; for let rec 1, probably can do something simpler?
//            // todo: later, can we avoid putting this onto the stack at all? maybe by editing environment
//              new Ref(name, () => Value(eval(cbinding, r, rec, x0, x1, x2, x3, stackU,
//                x0b, x1b, x2b, x3b, stackB, r), r.boxed))
//
//            eval(cbody, rec, U0, x0, x1, x2, pushU(stackU, x3),
//              evaledBinding, x0b, x1b, x2b, pushB(stackB, x3b), r)
//
//          }
        }
        */

      case Term.LetRec(bindings, body) => ???
    }
  }

  def hasTailRecursiveCall(rec: CurrentRec, term: Term): Boolean = ???

  @inline def eval(c: Computation, r: R, rec: Lambda, top: StackPtr,
                   stackU: Array[U], x1: U, x0: U,
                   stackB: Array[B], x1b: B, x0b: B): U =
    try c(r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
    catch { case TailCall => loop(r, top, stackU, stackB) }

  def loop(r: R, top: StackPtr, stackU: Array[U], stackB: Array[B]): U = {
    while (true) {
      try {
        // We've just caught a tail call - the arguments for the tail call are in `r`.
        // We copy these arguments to the current stack
        System.arraycopy(stackU, r.argsStart.toInt, stackU, top.toInt, r.stackArgsCount)
        System.arraycopy(stackB, r.argsStart.toInt, stackB, top.toInt, r.stackArgsCount)
        // ... and then null out the rest of the stack past the last argument
        // (todo: this is correct but maybe excessive - could be lazier or more targeted about nulling out the stack)
        java.util.Arrays.fill(
          stackB.asInstanceOf[Array[AnyRef]],
          top.toInt + r.stackArgsCount,
          stackB.length, null)

        return r.tailCall.body(r, r.tailCall, top.increment(r.stackArgsCount), stackU, r.x1, r.x0, stackB, r.x1b, r.x0b)
      }
      catch {
        case TailCall =>
      }
    }
    U0
  }

  // todo - could pass info here about the type of variable, whether it is boxed or unboxed, and optimize for this case
  def compileVar(e: Term, name: Name, env: Vector[Name], currentRec: CurrentRec): Computation =
    if (currentRec.contains(name))
      new Self(name)

    else env.indexOf(name) match {
      case -1 => sys.error("unbound name: " + name)
      case 0 => new Computation(e) {
        def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U =
          returnBoth(r, x0, x0b)
      }
      case 1 => new Computation(e) {
        def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U =
          returnBoth(r, x1, x1b)
      }
      case n => new Computation(e) {
        def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U =
          returnBoth(r, top.u(stackU, n), top.b(stackB, n))
      }
    }

  def compileRef(e: Term, name: Name, env: Vector[Name], currentRec: CurrentRec): ParamLookup = {
    if (currentRec.contains(name)) new ParamLookup {
      def apply(rec: Lambda, top: StackPtr, stackB: Array[B], x1b: B, x0b: B): B = rec
    }
    else env.indexOf(name) match {
      case -1 => sys.error("unbound name: " + name)
      case 0 => new ParamLookup {
        def apply(rec: Lambda, top: StackPtr, stackB: Array[B], x1b: B, x0b: B): B = x0b
      }
      case 1 => new ParamLookup {
        def apply(rec: Lambda, top: StackPtr, stackB: Array[B], x1b: B, x0b: B): B = x1b
      }
      case n => new ParamLookup {
        def apply(rec: Lambda, top: StackPtr, stackB: Array[B], x1b: B, x0b: B): B = top.b(stackB, n)
      }
    }
  }

  abstract class ParamLookup {
    def apply(rec: Lambda, top: StackPtr, stackB: Array[B], x1b: B, x0b: B): B
  }

}
