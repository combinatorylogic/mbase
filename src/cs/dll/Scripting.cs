//////////////////////////////////////////////////////////////////////////////
//
//   OpenMBase
//
// Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
// This file is distributed under the terms of the Q Public License version 1.0.
//
//////////////////////////////////////////////////////////////////////////////

using System;
using System.Collections;
using System.IO;
using System.Reflection;


namespace Meta.Scripting
{
#region Compiler
  public interface Code
  {
    Object run(Object[] frame, Object[] envframe);
  }
  public delegate Object SimpleCode(Object[] frame);

  public class Precompiler
  {
    static Symbol lambda = Symbol.make("lambda");
    static Symbol quote = Symbol.make("quote");
    static Symbol argref = Symbol.make("arg-ref");
    static Symbol envref = Symbol.make("env-ref");
    static Symbol begin = Symbol.make("begin");
    static Symbol theif = Symbol.make("if");
    static Symbol thetb = Symbol.make("top-begin");

    // non-bootstrap stuff: .net-specific
    static Symbol thetry = Symbol.make("try");
    ///////
    ///
    public Runtime rt;

    public static System.Collections.Generic.Dictionary<Symbol, object> symbols = new System.Collections.Generic.Dictionary<Symbol, object>();
    public static System.Collections.Generic.Dictionary<Symbol, object> macros = new System.Collections.Generic.Dictionary<Symbol, object>();
    public static SimpleCode wrap(Object o, int n)
    {
      MakeApp ap = new MakeApp(o, new Object[n]);
      return new SimpleCode(ap.run2);
    }
    public static Code compile(Object o)
    {
      try
        {
          if (o is Pair) return compile((Pair)o);
          else if (o is Symbol)
            {
              Symbol s = (Symbol)o;
              if (symbols.ContainsKey(s))
                return new MakeConstant(symbols[s]);
              else
                if (Runtime.comp_hash[((Symbol)o).v] != null) {
                  object valu = Runtime.makedelegate((Symbol)o);
                  Console.WriteLine("DELEGATE: " + o.ToString() + " --- " + valu.ToString());
                  symbols[s] = valu;
                  return new MakeConstant(valu);
                }
              {
                Console.WriteLine("global name " + s.v + " undefined");
                throw new
                  Meta.Scripting.MBaseException("global name " + 
                                                s.v + " undefined");
              }
            }
          else if (o is Code)
            {
              return (Code)o;
            }
          else return new MakeConstant(o);
        }
      catch (Exception e)
        {
          Console.WriteLine("Exception in " + (Runtime.print(o)));
          throw e;
        }
    }
    public static Code compile(Pair p)
    {
      if (p.caris(lambda))
        {
          int narg = ((System.Int32)p.caadr());
          Object fl = (p.cdadr());
          if (fl is Pair)
            {
              return new MakeFrameLambda(narg, (Pair)p.cdadr(), compile(p.caddr()));
            }
          else
            {
              return new MakeLambda(narg, compile(p.caddr()));
            }
        }
      else if (p.caris(quote))
        {
          return new MakeConstant(p.cadr());
        }
      else if (p.caris(argref))
        {
          return new MakeArgRef((Int32)(p.cadr()));
        }
      else if (p.caris(envref))
        {
          return new MakeEnvRef((Int32)(p.cadr()));
        }
      else if (p.caris(begin))
        {
          Pair l = p.pcdr;
          if (l.length() == 1)
            {
              return compile(p.cadr());
            }
          else
            {
              return new MakeSequence(p.pcdr);
            }
        }
      else if (p.caris(theif))
        {
          Object p1 = p.cadr();
          Object p2 = p.caddr();
          Object p3x = Pair.Cdr(p.cddr());
          Object p3;
          if (p3x != null) p3 = Pair.Car(p3x); else p3 = null;
          return new MakeIf(
                            compile(p1),
                            compile(p2),
                            (p3 == null) ? NIL : compile(p3), p);
        }
      else if (p.caris(thetry))
        {
          Object p1 = p.cadr();
          Pair p2 = (Pair)p.cddr();
          Object p2x = p2.car;
          Object p2y = p2.cadr();
          return new MakeTry(compile(p1), compile(p2x), compile(p2y));
        }
      else if (p.caris(thetb)) // suppress execution - already executed!
        {
          return NIL;
        }
      else  // it is a function application
        {
          return new MakeApp(p);
        }
    }
    private static Code NIL = new MakeConstant(null);
  }
#endregion

#region Evaluation atomic blocks
  class MakeTry : Code
  {
    Code ca, cex, cthen;
    public MakeTry(Code a, Code excpn, Code then)
    {
      ca = a; cex = excpn; cthen = then;
    }
    public Object run(Object[] frame, Object[] envframe)
    {
      try
        {
          return ca.run(frame, envframe);
        }
      catch (Exception ex)
        {
          Type oe = (Type)cex.run(frame, envframe);

          if (oe.IsAssignableFrom(ex.GetType()))
            {
              MakeApp ap = new MakeApp(cthen, new Object[] { ex });
              return ap.run(frame, envframe);
            }
          // otherwise:
          throw ex;
        }
    }
  }
  class MakeIf : Code
  {
    Code a, b, c;
    object _source;
    public MakeIf(Code cond, Code iftrue, Code iffalse, object src)
    {
      a = cond; b = iftrue; c = iffalse;
      _source = src;
    }
    public Object run(Object[] frame, Object[] envframe)
    {
      Object res = a.run(frame, envframe);
      if (res == null) return c.run(frame, envframe);
      else
        return b.run(frame, envframe);
    }
  }
  class MakeApp : Code
  {
    Object[] parts;
    public MakeApp(Pair p)
    {
      int l = p.length();
      parts = new Object[l];
      Pair pp = p;
      for (int i = 0; i < l; i++)
        {
          Object h = pp.car;
          pp = pp.pcdr;
          if (h is Pair)
            {
              parts[i] = Precompiler.compile(h);
            } else
                if (h is Symbol)
                  {
                    Symbol sh = (Symbol)h;
                    Object test;
                    Precompiler.symbols.TryGetValue(sh,out test);
                    if (test == null && !Precompiler.symbols.ContainsKey(sh))
                      {
                        if (Runtime.comp_hash[sh.v] != null)
                          {
                            test = Runtime.makedelegate(sh);
                          }
                        else
                          {
                            Console.WriteLine("sclmglobal name " + sh.v + " undefined");
                          }
                      }
                    parts[i] = test;
                  }
          else
            {
              parts[i] = h; // optimize constants
            }
        }
    }
    public MakeApp(Object f, Object[] args)
    {
      parts = new Object[args.Length + 1];
      for (int i = 1; i < args.Length + 1; i++) parts[i] = args[i - 1];
      parts[0] = f;
    }
    public Object run2(Object[] frame)
    {
      return run(frame, null);
    }
    public Object run(Object[] frame, Object[] envframe)
    {
      int len0 = parts.Length - 1;
      Object fun = runone(frame, envframe, parts[0]);
      bool isclosure = fun is Closure;
            
      Object[] newframe = new Object[len0];
      Object[] newenvframe;
      if (isclosure && ((Closure)fun).frame != null)
        {
          newenvframe = ((Closure)fun).frame;
        }
      else { newenvframe = envframe;}
      for (int i = 0; i < len0; i++)
        {
          newframe[i] = runone(frame, envframe, parts[i + 1]);
        }
      if (isclosure)
        {
          return ((Closure)fun).c.run(newframe, newenvframe);
        }
      if (fun is SimpleCode) return ((SimpleCode)fun)(newframe);
      if (fun is Code) return ((Code)fun).run(newframe, newenvframe);
      if (Runtime.clr != null)
        {
          Object o =
            Runtime.clr(newframe, fun);
          return o;
        }
      return fun; // error
    }
    private static Object runone(Object[] frame, Object[] envframe, Object part)
    {
      if (part is Code) return ((Code)part).run(frame, envframe);
      return part;
    }
  }
  class MakeSequence : Code
  {
    Code[] sqn;
    public MakeSequence(Pair p)
    {
      int l = p.length();
      sqn = new Code[l];
      Pair pp = p;
      for (int i = 0; i < l; i++)
        {
          Object h = pp.car;
          pp = pp.pcdr;
          sqn[i] = Precompiler.compile(h);
        }
    }
    public Object run(Object[] fr, Object[] envframe)
    {
      Object res = null;
      for (int i = 0; i < sqn.Length; i++)
        {
          res = sqn[i].run(fr, envframe);
        }
      return res;
    }
  }
  class MakeArgRef : Code
  {
    int i;
    public MakeArgRef(Int32 c)
    {
      i = c;
    }
    public Object run(Object[] frame, Object[] envframe)
    {
      return frame[i];
    }
  }
  class MakeEnvRef : Code
  {
    int i;
    public MakeEnvRef(Int32 c)
    {
      i = c;
    }
    public Object run(Object[] frame, Object[] envframe)
    {
      return envframe[i];
    }
  }
  class MakeConstant : Code
  {
    Object ob;

    public MakeConstant(Object o) { ob = o; }
    public Object run(Object[] o, Object[] envframe)
    {
      return ob;
    }
  }
  class MakeLambda : Code
  {
    int nars;
    Code code;
    public MakeLambda(int narg, Code c)
    {
      nars = narg;
      code = c;
    }
    public Object run(Object[] frame, Object[] envframe)
    {
      Closure cl = new Closure();
      cl.frame = null;
      cl.c = code;
      cl.nargs = nars;
      return cl;
    }
  }
  class MakeFrameLambda : Code
  {
    static Symbol thearg = Symbol.make("arg");
    static Symbol theenv = Symbol.make("env");
    static Symbol theself = Symbol.make("self");
    int flen;
    int nars;
    Code code;
    int[] patch;
    int[] epatch;

    public MakeFrameLambda(int narg, Pair alist, Code icode)
    {
      int alen = alist.length(); nars = narg;
      code = icode;
      flen = alen;
      patch = new int[alen];
      epatch = new int[alen];
      Pair c = alist;
      for (int i = 0; i < alen; i++)
        {
          object ccadr = ((Pair)c.car).cadr ();
          if (ccadr==thearg)
            {
              patch[i] = ((Int32)c.caar()); epatch[i] = -1;
            } else if(ccadr==theenv) {
            epatch[i] = ((Int32)c.caar()); patch[i] = -1;
          } else { // self
            epatch[i] = -1; patch[i] = -1;
          }
          c = (Pair)Pair.Cdr(c);
        }
    }
    public Object run(Object[] o,  Object[] envframe)
    {
      Object[] newframe = new Object[flen];
      Closure cl = new Closure();
      for (int j = 0; j < flen; j++)
        {
          int t = patch[j]; int p = epatch[j];
          if (t>=0) newframe[j] = o[t]; else if (p>=0) newframe[j] = envframe[p]; else {
            newframe[j] = cl;
          }
        }
      cl.nargs = nars;
      cl.c = code;
      cl.frame = newframe;
      return cl;
    }
  }
#endregion
}
