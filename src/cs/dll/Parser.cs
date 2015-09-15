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

#region S-Expr parser
  public class pospair
  {
    public string sfile;
    public int line;
    public int col;
    public string s;
    public pospair(string sf,int l, int c)
    {
      sfile = sf; line = l; col = c; s = null;
    }
  }
  public class Reader
  {
    static Symbol thequote = Symbol.make("quote");
    static Symbol theunquote = Symbol.make("unquote");
    static Symbol thesplicing = Symbol.make("unquote-splicing");
    static Symbol thequasiquote = Symbol.make("quasiquote");
    static Symbol annotatetag = Symbol.make("**with source info**");
    private static Object tryNum(string s)
    {
      try
        {
          return System.Int32.Parse(s, System.Globalization.CultureInfo.InvariantCulture.NumberFormat);

        }
      catch (Exception e)
        {
          return Symbol.make(s);
        }
    }
    public static Object process(Object o)
    {
      if (o is string)
        {
          string s = (string)o;
          if (s[0] == '"') return s.Substring(1, s.Length - 2);
          if (s[0] == '&')
            {
              if ((s.Length > 1) && s[1] == '\"')
                {
                  return Symbol.make(s.Substring(2, s.Length - 3));
                }
              else
                {
                  return Symbol.make(s);
                }
            }
          if (s[0] >= '0' && s[0] <= '9') return tryNum(s);
          if ((s[0] == '-' || s[0] == '+') && s.Length > 1 &&
              (s[1] >= '0' && s[1] <= '9')) return tryNum(s);
          if (s.Equals("#f")) return Runtime._false;
          if (s.Equals("#t")) return Runtime._true;
          if (s.StartsWith("#\\"))
            {
              if (s.Length == 3) return s[2];
              string ss = s.Substring(2, s.Length - 2);
              switch (ss)
                {
                case "Newline": return '\n';
                case "Tab": return '\t';
                case "Space": return ' ';
                case "LBR": return '(';
                case "RBR": return ')';
                case "Semicolon": return ';';
                }
              if (ss.Length > 5)
                {
                  if (ss.Substring(0, 5).Equals("ASCII"))
                    {
                      return (char)(Int32.Parse(ss.Substring(5, ss.Length - 5)));
                    }
                }
              if (s.Equals("#\\")) return '\\';
              return Symbol.make("Char(" + s + ")");
            }
          Symbol sym = Symbol.make(s);
          return sym;
        }
      else if (o is pospair)
        {
          pospair pp = (pospair)o;
          return new Pair(annotatetag, new Pair(new Pair(pp.sfile, new Pair(pp.line, new Pair(pp.col, null))),
                                                new Pair(process(pp.s), null)));
        }
      else if (o is Pair)
        {
          Pair p = (Pair)o;
          if (p.car != null)
            {
              string pc = null;
              if (p.car is string)
                {
                  pc = (string)p.car;
                }
              else if (p.car is pospair) pc = ((pospair)(p.car)).s;

              if (pc != null)
                {
                  Object oo = null;
                  if (pc.Equals("'")) oo = thequote;
                  else if (pc.Equals(",")) oo = theunquote;
                  else if (pc.Equals("`")) oo = thequasiquote;
                  else if (pc.Equals(",@")) oo = thesplicing;

                  if (oo != null)
                    return new Pair(new Pair(oo, new Pair(process(p.cadr()), null)),
                                    process(p.cddr()));
                }
            }
          p.car = process(p.car);
          p.cdr = process(p.cdr);
          return p;
        }
      return o;
    }

    private static bool popper(Stack st, Stack pos, Object ob)
    {
      try
        {
          if (pos.Count == 0) return false; // skip an operation

          int postn = (int)pos.Pop(); // get a position
          if (postn == 0)  // car
            {
              Pair p = (Pair)st.Peek(); // don't remove it!
              p.car = ob;
              pos.Push(2); // by default - continue this list
            }
          else if (postn == 1) // cdr
            {
              Pair p = (Pair)st.Peek();
              p.cdr = ob;
              pos.Push(-1); // don't update at all
            }
          else if (postn == 2)
            {
              Pair p = (Pair)st.Pop(); // don't need it any more
              Pair p1 = new Pair(ob, null);
              p.cdr = p1;
              st.Push(p1);
              pos.Push(2); // continue this way
            }
          return true;
        }
      catch (System.InvalidOperationException ex)
        {
          throw new Meta.Scripting.MBaseException(new Pair(Symbol.make("READER-ERROR-AT:"), ob));
        }
    }

    public static Object aread(System.IO.TextReader r)
    {
      return aread(new ExtendedReader(r));
    }
    public static Object aread(ExtendedReader r)
    {
      Object prev_p = null;
      Stack top = new Stack();
      char[] c = new char[1];
      bool rd = true;
      bool annotate = r.annotate;
      int state = 0;
      string s = "";
      pospair strt = null;
      pospair crnt = null;
      System.Collections.Stack st = new System.Collections.Stack();
      System.Collections.Stack pos = new System.Collections.Stack();
      while (true)
        {
          char ch;
          if (!rd) { if (top.Count > 0) return top.Pop(); else return null; }
          int n = r.Read(c, 0, 1);
          if (annotate) crnt = new pospair(r.file, r.linepos, r.cpos);
          if (n == 0)
            {
              rd = false; // stop
              ch = ' '; // simulate end of something
              goto cont;
            }
          ch = c[0]; //Console.Write(ch);
          cont:

          if (state == 0)
            {
              switch(ch) {
              case '(':
                {
                  // start a new list;
                  Pair p = new Pair(null, null);

                  top.Push(p);
                  st.Push(p);
                  pos.Push(0); // will fill car first
                  prev_p = p;
                  break;
                }
              case ')':
                {
                  // pop a list
                  if (top.Count < 1)
                    {
                      throw new
                        Meta.Scripting.MBaseException(new Pair(Symbol.make("READER-ERROR-AT:"),
                                                               prev_p));
                    }
                  Object p = top.Pop();
                  if (p is Pair)
                    {
                      Pair pp = (Pair)p;
                      if (pp.car == null && pp.cdr == null) p = null;
                    }
                  st.Pop(); // discard it
                  pos.Pop(); //discard it
                  if (!popper(st, pos, p))
                    {
                      return p;
                    }
                  if (p != null) prev_p = p;
                  state = 0;
                  break;
                }
              case '.':
                {
                  s = "" + ch; strt = crnt; state = 6;
                  break;
                }
              case ';':
                {
                  state = 4; // comment started
                  break;
                }
              case ' ':
              case '\n':
              case '\t':
              case '\r':
              case '\f':
                { // just skip
                  break;
                }
              case '\"': // start a string literal
                {
                  s = "" + ch; strt = crnt;
                  state = 2;
                  break;
                }
              case '\'':
              case '`': // immediate
                {
                  s = "" + ch; strt = crnt;
                  if (!popper(st, pos, s)) return s;
                  s = "";
                  break;
                }
              case ',':  // delay
                {
                  s = "" + ch; strt = crnt;
                  state = 5;
                  break;
                }
              case '&': // another delayed reader
                {
                  s = "" + ch; strt = crnt;
                  state = 7;
                  break;
                }
              default: // all other chars starts atoms
                {
                  s = "" + ch; // start a new one;
                  strt = crnt;
                  state = 1; // atom processing
                  break;
                }
              }
            }
          else if (state == 1)
            {
              if (ch == '(' || ch == ')' || Char.IsWhiteSpace(ch) || ch == ';')
                {
                  // atom ends here, sorry
                  if (annotate)
                    {
                      strt.s = s;
                      if (!popper(st, pos, strt)) return strt;
                    }
                  else
                    {
                      if (!popper(st, pos, s)) return s;
                    }
                  if(s!=null) prev_p = s;
                  s = "";
                  state = 0;
                  goto cont; // now process the termination character in a new state
                }
              else s = s + ch; // add a char to an atom
            }
          else if (state == 2)
            {
              if (ch == '\\') { state = 3; }
              else if (ch == '\"')
                {
                  s = s + ch;
                  state = 1;
                  ch = ' '; // simulate an end of atom
                  goto cont;
                }
              else
                {
                  s = s + ch;
                }
            }
          else if (state == 3)
            {
              if (ch == 'n') ch = '\n';
              else if (ch == 't') ch = '\t';
              s = s + ch; state = 2;
            }
          else if (state == 4)
            {
              if (ch == '\n') state = 0; // comment line finished
            }
          else if (state == 5)
            {
              if (ch == '@') s += ch;

              if (!popper(st, pos, s)) return s;
              state = 0;
              if (ch != '@') goto cont; // otherwise read next char
            }
          else if (state == 6)
            {
              if (ch == '.') { s += ch; state = 1; }
              else
                {
                  state = 0;
                  pos.Pop();
                  pos.Push(1); // will fill cdr, not a new cons cell
                  goto cont;
                }
            }
          else if (state == 7)
            {
              if (ch == '\"')
                {
                  s += ch;
                  state = 2;
                }
              else
                {
                  state = 1;
                  goto cont;
                }
            }
        }
    }


  }
#endregion

}