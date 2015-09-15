//////////////////////////////////////////////////////////////////////////////
//
//   OpenMBase
//
// Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
//
//
//////////////////////////////////////////////////////////////////////////////

using System;
using System.Collections;
using System.IO;
using System.Reflection;


namespace Meta.Scripting
{
#region Aux stuff
  public class MBaseException : Exception
  {
    private Object x = null;
    public MBaseException(Object o)
    {
      x = o;
    }
    public Object val()
    {
      return x;
    }
  }

  public class AltClosure { }

  public class NativeClosure
  {
    public Object[] frame;
    public IntPtr fun;
    public NativeClosure(Object[] fr, IntPtr fn)
    {
      frame = fr; fun = fn;
    }
    public Object MakeRec()
    {
      frame[0] = this;
      return this;
    }
    public Object HackClosure(int n, Object v)
    {
      frame[n] = v;
      return this;
    }

  }

  public delegate Object MiscCall(Object[] frame, Object ptr);

  public class Pair : ICollection, IEnumerable
  {
    public Object car, cdr;
    public Pair(Object a, Object b)
    {
      car = a; cdr = b;
    }

    public override string ToString()
    {
      return Runtime.print(this);
    }

    public bool caris(Symbol s)
    {
      if (car != null && car is Symbol)
        {
          return car == s;
        }
      return false;
    }
    public static Object Car(Object p)
    {
      if (p == null) return null;
      if (p is Pair)
        {
          Pair pp = (Pair)p;
          return pp.car;
        }
      else return null;
    }

    public static Object Cdr(Object p)
    {
      if (p == null) return null;
      if (p is Pair)
        {
          Pair pp = (Pair)p;
          return pp.cdr;
        }
      else return null;
    }
    public Object cadr()
    {
      return Car(Cdr(this));
    }
    public Object caar()
    {
      return Car(Car(this));
    }
    public Object caddr()
    {
      return Car(cddr());
    }
    public Object cddr()
    {
      return Cdr(Cdr(this));
    }
    public Object caadr()
    {
      return Car(cadr());
    }
    public Object cdadr()
    {
      return Cdr(cadr());
    }
    public int length()
    {
      Object o = this;
      int n = 1;
      while (true)
        {
          o = Cdr(o);
          if (!(o is Pair)) return n;
          n++;
        }
    }

    public Pair pcar
    {
      get { return (Pair)car; }
      set { car = (object)pcar; }
    }

    public Pair pcdr
    {
      get { return (Pair)cdr; }
      set { car = (object)pcdr; }
    }

    // ICollection methods
    public int Count
    {
      get
        {
          return length();
        }
    }

    public void CopyTo(Array ar, int idx)
    {
      Pair p = this;
      int pos = idx;
      for (; ; )
        {
          ar.SetValue(p.car, pos);
          Object nxt = p.cdr;
          if (nxt != null && nxt is Pair)
            {
              p = (Pair)nxt; pos++;
            }
          else return;
        }
    }

    public object SyncRoot
    {
      get
        {
          return this;
        }
    }

    public bool IsSynchronized
    {
      get
        {
          return false;
        }
    }

    // IEnumerable methods

    public IEnumerator GetEnumerator()
    {
      return new PairEnumerator(this);
    }

  }

  public class PairEnumerator : IEnumerator
  {
    private Pair head, crnt;
    public PairEnumerator(Pair p)
    {
      head = p; crnt = null;
    }

    public object Current
    {
      get
        {
          return crnt.car;
        }
    }

    public bool MoveNext()
    {
      if (crnt == null)
        {
          crnt = head; return true;
        }
      Object nxt = crnt.cdr;
      if (nxt != null && nxt is Pair)
        {
          crnt = (Pair)nxt;
          return true;
        }
      else
        {
          crnt = null;
          return false;
        }
    }

    public void Reset()
    {
      crnt = head;
    }
  }

  public class ExtendedReader
  {
    private Object hint;
    private int oldlinepos, oldcpos;
    public int linepos, cpos;
    public string file;
    private System.IO.TextReader rdr;
    public ExtendedReader(System.IO.TextReader r)
    {
      rdr = r;
      linepos = 0;
      oldlinepos = 0;
      cpos = 0;
      oldcpos = 0;
      file = null;
    }
    public bool annotate = false;
    public void setannotate(string fname)
    {
      annotate = true; file = fname;
    }
    public int Read(char[] c, int a, int b)
    {
      int res = rdr.Read(c, a, b);
      if (annotate)
        {
          for (int i = 0; i < res; i++)
            {
              updpos(c[a + i]);
            }
        }
      return res;
    }
    private void updpos(char c)
    {
      oldlinepos = linepos; oldcpos = cpos;
      if (c == '\n')
        {
          linepos++;
          cpos = 0;
        }
      else cpos++;
    }
    public int Read()
    {
      int res = rdr.Read();
      if (annotate)
        {
          char ch = (char)res;
          updpos(ch);
        }
      return res;
    }
    public int Peek()
    {
      if (annotate)
        {
          linepos = oldlinepos; cpos = oldcpos;
        }
      return rdr.Peek();
    }
    public void Close()
    {
      rdr.Close();
    }

    public static ExtendedReader mkreader(System.IO.TextReader r)
    {
      return new ExtendedReader(r);
    }
    public void mkhint(Object o) { hint = o; }
    public Object gethint() { return hint; }
  }

  public class ASymbol
  {
    public Symbol s;
    public object mdata;
  }

  public class Symbol
  {
    public string v;
    public static System.Collections.Generic.Dictionary<string, Symbol> h = new System.Collections.Generic.Dictionary<string, Symbol>();

    public static Symbol make(string s)
    {
      Symbol sym;
      h.TryGetValue(s,out sym);
      if (sym == null)
        {
          sym = new Symbol(); sym.v = s;
          h[s] = sym;
        }
      return sym;
    }

    public int hash(int n)
    {
      return 0;
    }
    public override string ToString()
    {
      return v;
    }

  }

  public class Closure
  {
    public int nargs;
    public Object[] frame;
    public Code c;
  }
#endregion
}


