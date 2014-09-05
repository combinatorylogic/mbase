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
using System.Reflection.Emit;
using System.Diagnostics.SymbolStore;
using System.Diagnostics;




namespace Meta.Scripting
{

  public sealed class Runtime
  {

#region Initialization code
    private static void init()
    {
      Precompiler.symbols[_ss("*MODULE-SEARCH-PATH*")] =
        new Pair(Symbol.make("bootlib"),
                 new Pair(Symbol.make("corelib"), null));
      Precompiler.symbols[_ss("nil")] = null;    
      Precompiler.symbols[_ss("bool-false")] = _false;
      n("+", new SimpleCode(add), "_add");  
      n("-", new SimpleCode(sub), "_sub");  

      n(">", new SimpleCode(gt), "_gt"); 
      n("<", new SimpleCode(lt), "_lt"); 
      n(">=", new SimpleCode(ge), "_ge"); 
      n("<=", new SimpleCode(le), "_le"); 
      n("=", new SimpleCode(eq), "_eq");  
      n("eqv?", new SimpleCode(eqv), "_eqv"); 
      n("eq?", new SimpleCode(eqp), "_eqp");  

      n("cons", new SimpleCode(cons), "_cons"); 
      n("car", new SimpleCode(car), "_car");  
      n("cdr", new SimpleCode(cdr), "_cdr");

      n("set-cdr!", new SimpleCode(setcdr)); 
      n("set-car!", new SimpleCode(setcar)); 


      n("null?", new SimpleCode(nullp), "_nullp"); 
      n("list?", new SimpleCode(pairp), "_Pairp"); 
      n("pair?", new SimpleCode(pairp), "_Pairp"); 
      n("string?", new SimpleCode(stringp), "_stringp"); 
      n("symbol?", new SimpleCode(symbolp), "_symbolp"); 
      n("char?", new SimpleCode(charp), "_charp"); 
      n("number?", new SimpleCode(numberp), "_numberp"); 
      n("boolean?", new SimpleCode(booleanp), "_booleanp"); 

      n("symbol->string", new SimpleCode(any2string), "_any2string"); 
      n("any->string", new SimpleCode(any2string), "_any2string");    
      n("string->symbol", new SimpleCode(string2symbol), "_string2symbol"); 
      n("string-append", new SimpleCode(stringappend), "_stringappend");

      n("ascii", new SimpleCode(ascii), "_ascii"); 
      n("mkchar", new SimpleCode(mkchar), "_mkchar");

      n("string->list", new SimpleCode(string_to_list), "_string_to_list"); 
      n("list->string", new SimpleCode(list_to_string), "_list_to_string"); 


      n("eval", new SimpleCode(eval));

      n("mkhash", new SimpleCode(mkhash), "_mkhash"); 
      n("mkshash", new SimpleCode(mkshash), "_mkshash"); 
      n("ohashget", new SimpleCode(ohashget), "_ohashget"); 
      n("hashget", new SimpleCode(hashget), "_hashget"); 
      n("ohashput", new SimpleCode(ohashput), "_ohashput"); 
      n("hashput", new SimpleCode(hashput), "_hashput"); 
      n("shashget", new SimpleCode(shashget), "_shashget"); 
      n("shashput", new SimpleCode(shashput), "_shashput");
      
      
      n("def", new SimpleCode(def), "_ccdef"); 
      n("defmacro", new SimpleCode(defmacro), "_ccdefmacro"); 
      n("ccdef", new SimpleCode(__ccdef), "_ccdef");
      n("ccdefmacro", new SimpleCode(defmacro), "_ccdefmacro");
      n("getmacroenv", new SimpleCode(getmacroenv));
      n("getfuncenv", new SimpleCode(getfuncenv));

      n("symbol-starts-with", new SimpleCode(symbol_starts_with), "_symbol_starts_with"); 
      n("string-escape", new SimpleCode(stringescape)); 

      initreflection();
    }
    
    private static void initreflection()
    {
      n("aget", new SimpleCode(aget), "_aget"); 
      n("aset", new SimpleCode(aset), "_aset"); 
      n("anew", new SimpleCode(anew), "_anew"); 
      n("alength", new SimpleCode(alength), "_alength"); 
      n("mkvector", new SimpleCode(vector), "_vector");  
      n("mkovector", new SimpleCode(ovector), "_ovector");
      
      n("mkbytevector", new SimpleCode(mkbytevector), "_mkbytevector"); 

      n("r_gettype", new SimpleCode(getType), "_getType");
      n("r_typename", new SimpleCode(getTypeName), "_getTypeName");
      n("r_typebyname", new SimpleCode(getTypeByName), "_getTypeByName");
      n("r_typeshortname", new SimpleCode(getTypeSName), "_getTypeSName");
      n("r_getmethod0", new SimpleCode(getMethod0), "_getMethod0");
      n("r_invoke", new SimpleCode(invoke), "_invoke");
      n("r_new", new SimpleCode(r_new), "_r_new");
      n("r_raise", new SimpleCode(r_raise), "_r_raise");
      n("r_basepath", new SimpleCode(r_basepath));
      n("r_boolmethodp", new SimpleCode(r_boolmethodp), "_r_boolmethodp");
      n("r_debool", new SimpleCode(r_debool), "_r_debool");
      initextra();
    }
    
    private static void initextra()
    {
      Precompiler.symbols[_ss("*asms*")] = assmblys;

      n("*", new SimpleCode(mul), "_mul"); 
      n("/", new SimpleCode(div), "_div"); 
      n("%", new SimpleCode(mod));         

      n("number->string", new SimpleCode(any2string), "_any2string");

      n("set-lookup-path", new SimpleCode(setXpath),"_setXpath"); 
      n("get-lookup-path", new SimpleCode(getXpath),"_getXpath"); 

      n("bitand", new SimpleCode(bitand), "_bitand"); 
      n("bitor", new SimpleCode(bitor), "_bitor");    

      n("symhash", new SimpleCode(symhash), "_symhash"); 

      n("print", new SimpleCode(tprint),"_tprint"); 

      n("println", new SimpleCode(tprintln),"_tprintln"); 

      n("int:apply", new SimpleCode(int_apply));

      n("load-alc", new SimpleCode(runalcfile), "_runalcfile");

      Precompiler.symbols[_ss("t_Exception")] = typeof(System.Exception);
      Precompiler.symbols[_ss("t_MBaseException")] = typeof(MBaseException);
    }
#endregion


#region Misc definitions
    public static string prompt = "> ";
    public static string rprompt = "< ";
    private static TextWriter con = System.Console.Out;

    private static string _areadstr = null;

    public static Object Apply(Object f, Object[] args)
    {
      MakeApp ap = new MakeApp(f, args);
      return ap.run(null, null);
    }


    public static Object aread(ExtendedReader rdr)
    {
      if (_areadstr == null)
        {
          return (Reader.process(Reader.aread(rdr)));
        }
      else
        {
          return apply(_ss(_areadstr), new Object[] { rdr });
        }
    }

    public static MiscCall clr = null; // misc caller to be registered
    // later from the interpreted code.

    public static void iRuntime()
    {
      Runtime_init(true);
    }

    static Assembly localResolver(object sender, ResolveEventArgs args)
    {
      string name = args.Name;
      object res = ashash2[name];
      if (res != null) return (Assembly)res;
      return null;
    }


    public static void Runtime_init(bool dflt)
    {
      AppDomain currentDomain = AppDomain.CurrentDomain;

      currentDomain.AssemblyResolve +=
        new ResolveEventHandler(localResolver);

      Precompiler.symbols[_ss("*CMDLINE*")] = new string[0];
      init();
    }

    private static Hashtable hashclone(Hashtable h)
    {
      Hashtable h2 = new Hashtable();
      foreach (Object k in h.Keys)
        {
          Object v = h[k];
          if (v is Hashtable) h2[k] = hashclone((Hashtable)v);
          else
            h2[k] = v;
        }
      return h2;
    }

    public static void setargs(string[] str)
    {
      Precompiler.symbols[_ss("*CMDLINE*")] = str;
    }

    public static void setConsole(TextWriter tw)
    {
      con = tw;
    }

    public static Object apply(Symbol fn, Object[] args)
    {
      Object cd = Precompiler.symbols[fn];
      MakeApp ap = new MakeApp(cd, args);
      return ap.run(null, null);
    }

    private static Symbol _to_string = _ss("to-string");

    public static string print(Object o)
    {
      return (string)apply(_to_string, new Object[] { o });
    }


    public static void InitModule()
    {
      apply(_ss("read-int-eval"), new Object[] { read("(n.module DEFAULT)") });
    }

    private static Symbol _read_compile_eval = _ss("read-compile-eval");
    public static Object eval(string str)
    {
      ExtendedReader r = new ExtendedReader(new System.IO.StringReader(str));
      Object o = aread(r);
      Object result =
        apply(_read_compile_eval,
              new Object[] { o });
      return result;
    }

    public static Object meval(string str)
    {
      Object result = null;
      ExtendedReader tr = new ExtendedReader(new System.IO.StringReader(str));
      while (true)
        {
          Object o = aread(tr);
          if (o == null) return result;
          result =
            apply(_read_compile_eval,
                  new Object[] { o });
        }
    }

    public static Object eval0(string str)
    {
      ExtendedReader r = new ExtendedReader(new System.IO.StringReader(str));
      Object o = aread(r);
      Object result = (Precompiler.compile(o).run(null, null));
      return result;
    }

    public static void rteval(string str, System.IO.TextWriter ot)
    {
      TextWriter tw0 = con;
      con = ot;
      try
        {
          ot.WriteLine("<<< " + str);

          Object o = str;
          Object result =
            apply(_ss("p-read-compile-eval"),
                  new Object[] { o });
          Object resx = apply(_to_string, new Object[] { result });
          ot.WriteLine(">>> " + resx);
        }
      catch (Exception e)
        {
          ot.WriteLine(e.ToString());
        }
      con = tw0;
    }
    public static Object read(string str)
    {
      ExtendedReader r = new ExtendedReader(new System.IO.StringReader(str));
      Object p = aread(r);
      r.Close();
      return p;
    }

    public static void readfile(string nm)
    {
      StreamReader tr = new StreamReader(nm);
      xpath = Path.GetDirectoryName(nm);
      if (xpath.Equals(""))
        {
          xpath = Directory.GetCurrentDirectory();
        }
      else xpath = Path.GetFullPath(xpath);
      while (true)
        {
          string s =
            tr.ReadLine();
          if (s == null) break;
          try
            {
              eval0(s);
            }
          catch (Exception e)
            {
              con.WriteLine(e.StackTrace);
              tr.Close();
              return;
            }
        }
      tr.Close();
    }


    public static void runfile(string nm)
    {
      StreamReader sr = new StreamReader(nm);
      xpath = Path.GetDirectoryName(nm);
      runfile(sr);
      sr.Close();
    }

    public static void runfile(TextReader tr0, bool prmpt)
    {
      runfile("read-compile-eval", tr0, prmpt);
    }

    public static void runfile(string cfun0, TextReader tr0, bool prmpt)
    {
      ExtendedReader tr = new ExtendedReader(tr0);
      Symbol cfun = _ss(cfun0);
      while (true)
        {
          if (prmpt) { con.Write(prompt); con.Flush(); }
          Object o = aread(tr);
          if (o == null) return;
          try
            {
              Object result =
                apply(cfun,
                      new Object[] { o });
              if (prmpt)
                {
                  con.WriteLine(rprompt + print(result));
                  con.Flush();
                }
            }
          catch (Exception e)
            {
              con.WriteLine("Exception " + e + "\nin: \n" + print(o));
              con.Flush();
            }
        }
    }
    public static void runfile(StreamReader tr)
    {
      runfile(tr, false);
    }

    public static string binpath(string pth)
    {
      string bpth =
        Path.Combine(
                     Path.GetDirectoryName(
                                           Assembly.GetExecutingAssembly().Location), pth);
      return bpth;
    }

    private static System.Collections.ArrayList asmbls = new System.Collections.ArrayList();
    private static Hashtable hasmbls = new Hashtable();

    private static void addassmbly(Assembly a)
    {
      string nm = a.GetName().Name;
      if (nm.EndsWith("_CT")) return; // convention!
      if (hasmbls[nm] != null) return;
      hasmbls[nm] = a;
      asmbls.Add(a);
    }

    private static Assembly[] getassmblys()
    {
      return (Assembly[])asmbls.ToArray(typeof(Assembly));
    }

    private static Hashtable ashash2 = new Hashtable();

    private static Assembly loaddll(string path)
    {
      Assembly a = null;

      a = Assembly.LoadFile(System.IO.Path.GetFullPath(path));

      if (a == null) return a;
      assmblys[a.GetName().Name] = a;
      addassmbly(a);
      ashash2[a.FullName] = a;
      return a;
    }

    public static void rundll(string path)
    {
      Assembly a = Assembly.LoadFile(System.IO.Path.GetFullPath(path));
      a.FullName.Substring(a.FullName.IndexOf(", ") + 2);
      runassembly(a);
    }

    public static void runassembly(Assembly a)
    {
      if (a != null)
        {
          string anm = a.GetName().Name + ".InitDLL";
          Type tp = a.GetType(anm);
          MethodBase im = tp.GetMethod("init");
          im.Invoke(null, new object[] { });
          Runtime.assmblys[a.GetName().Name] = a;
        }
    }

    public static void runmain()
    {
      apply(_ss("main"), new Object[] { });
    }

    private static Symbol _ss(string s) { return Symbol.make(s); }

    static void n(string s0, SimpleCode c)
    {
      Symbol s = Symbol.make("corelib:" + s0);
      Precompiler.symbols[s] = c;
    }

    static void n(string s0, SimpleCode c, string nativ)
    {
      Symbol s = Symbol.make("corelib:"+s0);
      Precompiler.symbols[s] = c;
      nativs[s] = nativ;
    }


    public static Hashtable nativs = new Hashtable();
    public static Hashtable assmblys = new Hashtable();

    public static string xpath = "./";
    public static Object setXpath(Object[] p)
    {
      return _setXpath(p[0]);
    }

    public static Object _setXpath(Object p0)
    {
      string s = (string)p0;
      if (s.Equals(""))
        {
          xpath = System.IO.Directory.GetCurrentDirectory();
        }
      else
        {
          xpath = System.IO.Path.GetFullPath(s);
        }
      return null;
    }

    public static Object getXpath(Object[] p)
    {
      return _getXpath();
    }

    public static Object _getXpath()
    {
      return xpath;
    }

#endregion

#region Functions

    static Object runalcfile(Object[] f)
    {
      return _runalcfile(f[0]);
    }

    public static Object _runalcfile(Object fn)
    {
      readfile((string)fn);
      return null;
    }

    static Object setcar(Object[] f)
    {
      ((Pair)(f[0])).car = f[1];
      return null;
    }

    static Object setcdr(Object[] f)
    {
      ((Pair)(f[0])).cdr = f[1];
      return null;
    }

    static Object symbol_starts_with(Object[] f)
    {
      return _symbol_starts_with(f[0], f[1]);
    }
    public static Object _symbol_starts_with(Object f0, Object f1)
    {
      string s0 = f0.ToString();
      string s1 = f1.ToString();
      if (s1.StartsWith(s0)) return _true;
      return null;
    }

    static Object int_apply(Object[] f)
    {
      MakeApp ap = new MakeApp(f[0], (Object[])(f[1]));
      return ap.run(null, null);
    }

    static Object r_basepath(Object[] f)
    {
      return Path.GetFullPath(
                              Path.Combine(
                                           Path.Combine(
                                                        Path.GetDirectoryName(Assembly.GetExecutingAssembly().
                                                                              Location),
                                                        ".." + Path.DirectorySeparatorChar + "lib"),
                                           (string)f[0]));
    }

    static Object r_raise(Object[] f)
    {
      throw (Exception)(f[0]);
    }

    public static Object _r_raise(Object e)
    {
      throw (Exception)e;
    }

    static Object r_new(Object[] f)
    {
      return _r_new(f[0], f[1], f[2]);
    }
    public static Object _r_new(Object f0, Object f1, Object f2)
    {
      ConstructorInfo ci = ((Type)f0).GetConstructor((Type[])f1);
      try
        {
          return ci.Invoke((Object[])f2);
        }
      catch (TargetInvocationException e)
        {
          throw e.InnerException;
        }
    }

    static Object tprint(Object[] f)
    {
      return _tprint(f[0]);
    }
    static public Object _tprint(Object f0)
    {
      con.Write(f0);
      con.Flush();
      return null;
    }

    static Object tprintln(Object[] f)
    {
      return _tprintln(f[0]);
    }
    static public Object _tprintln(Object f0)
    {
      con.WriteLine(f0);
      con.Flush();
      return null;
    }
    static Object alength(Object[] f)
    {
      return _alength(f[0]);
    }
    public static Object _alength(Object f0)
    {
      if (f0 is Object[])
        {
          return ((Object[])f0).Length;
        }
      else if (f0 is Array)
        {
          return ((Array)f0).GetLength(0);
        }
      return -1;
    }


    // array, n
    static Object aget(Object[] f)
    {
      return _aget(f[0], f[1]);
    }
    public static Object _aget(Object f0, Object f1)
    {
      int i = (int)f1;
      if (f0 is Object[])
        {
          Object[] ar = (Object[])f0;
          return ar[i];
        }
      else if (f0 is Array)
        {
          return ((Array)f0).GetValue(i);
        }
      return null;
    }

    // array, n, v
    static Object aset(Object[] f)
    {
      return _aset(f[0], f[1], f[2]);
    }
    public static Object _aset(Object f0, Object f1, Object f2)
    {
      if (f0 is Object[])
        {
          ((Object[])f0)[(int)f1] = f2;
        }
      else if (f0 is Array)
        {
          ((Array)f0).SetValue(f2, (int)f1);
        }
      return null;
    }

    // (Type,int)
    static Object anew(Object[] f)
    {
      return _anew(f[0], f[1]);
    }
    public static Object _anew(Object f0, Object f1)
    {
      Type t = (Type)f0;
      return System.Array.CreateInstance(t, (int)f1);
    }

    // list
    static Object vector(Object[] f)
    {
      return _vector(f[0]);
    }
    public static Object _vector(Object f0)
    {
      Pair p = (Pair)f0;
      Type t = null;
      if (p != null && p.car != null) t = p.car.GetType(); else t = Type.GetType("System.Object");
      if ( p == null || p.car == null )
        return Array.CreateInstance(t, 0);
                
      int l = p.length();
      Array ar =
        Array.CreateInstance(t, l);
      for (int i = 0; i < l; i++)
        {
          ar.SetValue(p.car, i); p = (Pair)p.cdr;
        }
      return ar;
    }
    // list
    static Object ovector(Object[] f)
    {
      return _ovector(f[0]);
    }
    public static Object _ovector(Object f0)
    {
      Pair p = (Pair)f0;

      if ( p == null || p.length() == 0 )
        return new Object[0];

      int l = p.length();
      Object[] ar = new Object[l];
      for (int i = 0; i < l; i++)
        {
          ar[i] = p.car; p = (Pair)p.cdr;
        }
      return ar;
    }
    // Type, string, Type[]

    static Object getMethod0(Object[] f)
    {
      return _getMethod0(f[0], f[1], f[2]);
    }
    public static Object _getMethod0(Object f0, Object f1, Object f2)
    {
      Type t = (Type)f0;
      MethodInfo mi;
      Type[] tt = { };
      if (f2 != null)
        mi = t.GetMethod((string)f1, (Type[])(f2));
      else
        mi = t.GetMethod((string)f1, tt);
      return mi;
    }

    // MethodInfo, Object(inst), Object[] args
    static Object invoke(Object[] f)
    {
      return _invoke(f[0], f[1], f[2]);
    }
    public static Object _invoke(Object f0, Object f1, Object f2)
    {
      MethodInfo mi = (MethodInfo)f0;
      try
        {
          return mi.Invoke(f1, (Object[])f2);
        }
      catch (TargetInvocationException e)
        {
          FieldInfo rts = typeof(Exception).GetField("_remoteStackTraceString",
                                                     BindingFlags.Instance | BindingFlags.NonPublic);

          rts.SetValue(e.InnerException,
                       e.InnerException.StackTrace + Environment.NewLine);

          throw e.InnerException;
        }
    }

    static Object r_boolmethodp(Object[] f)
    {
      return _r_boolmethodp(f[0]);
        
    }

    public static object _r_boolmethodp(object f0)
    {
      MethodInfo mi = (MethodInfo)f0;
      if (mi.ReturnType == true.GetType()) return _true;
      return null;
    }

    static object r_debool(Object[] f)
    {
      return _r_debool(f[0]);
    }

    public static object _r_debool(object f)
    {
      if ((Boolean)f) return _true;
      return null;
    }
     

    static Object getType(Object[] f)
    {
      return f[0].GetType();
    }

    public static Object _getType(Object f0)
    {
      return f0.GetType();
    }

    static Object getTypeName(Object[] f)
    {
      Type t = (Type)f[0];
      return t.FullName;
    }

    public static Object _getTypeName(Object tp)
    {
      Type t = (Type)tp;
      return t.FullName;
    }
    // string
    static Object getTypeByName(Object[] f)
    {
      string nm = (string)f[0];
      return Type.GetType(nm);
    }
    public static Object _getTypeByName(Object tpn)
    {
      string nm = (string)tpn;
      return Type.GetType(nm);
    }

    public static Object symhash(Object[] v)
    {
      return _symhash(v[0], v[1]);
    }
    public static Object _symhash(Object sym, Object lev)
    {
      return (Object)(((Symbol)sym).hash((int)lev));
    }
    public static Object mkbytevector(Object[] v)
    {
      return _mkbytevector(v[0]);
    }
    public static Object _mkbytevector(Object data)
    {
      Pair p = (Pair)data;
      int l = p.length();
      byte[] b = new byte[l];
      int i = 0;
      foreach (Object bb in p)
        {
          b[i] = (byte)((int)bb);
          i++;
        }
      return b;
    }            

    static Object getTypeSName(Object[] f)
    {
      return ((Type)f[0]).Name;
    }
    public static Object _getTypeSName(Object tp)
    {
      return ((Type)tp).Name;
    }
#endregion

#region Core functions

    static Object eqp(Object[] f)
    {
      if (f[0] == null || f[1] == null) return null;
      if (f[0].Equals(f[1])) return _true;
      return null;
    }
    static Object eqv(Object[] f)
    {
      if (f[0] == f[1]) return _true;
      return null;
    }
    static Object eq(Object[] f)
    {
      return _eq(f[0], f[1]);
    }
    static Object le(Object[] f)
    {
      int a = (int)f[0];
      int b = (int)f[1];
      if (a <= b) return _true;
      return null;
    }

    static Object ge(Object[] f)
    {
      int a = (int)f[0];
      int b = (int)f[1];
      if (a >= b) return _true;
      return null;
    }

    static Object lt(Object[] f)
    {
      int a = (int)f[0];
      int b = (int)f[1];
      if (a < b) return _true;
      return null;
    }

    static Object gt(Object[] f)
    {
      int a = (int)f[0];
      int b = (int)f[1];
      if (a > b) return _true;
      return null;
    }

    //-<
    public static Object _eqp(Object f0, Object f1)
    {
      if (f0 == null || f1 == null) return null;
      if (f0.Equals(f1)) return _true;
      return null;
    }
    public static Object _eqv(Object f0, Object f1)
    {
      if (f0 == f1) return _true;
      return null;
    }
    public static Object _eq(Object f0, Object f1)
    {
      int a = (int)f0; int b = (int)f1;
      if (a == b) return _true;
      return null;
    }
    public static Object _le(Object f0, Object f1)
    {
      int a = (int)f0;
      int b = (int)f1;
      if (a <= b) return _true;
      return null;
    }

    public static Object _ge(Object f0, Object f1)
    {
      int a = (int)f0;
      int b = (int)f1;
      if (a >= b) return _true; else return null;
    }

    public static object _true = (Object)true;
    public static object _false = (Object)false;

    public static Object _lt(Object f0, Object f1)
    {
      int a = (int)f0;
      int b = (int)f1;
      if (a < b) return _true;
      return null;
    }

    public static Object _gt(Object f0, Object f1)
    {
      int a = (int)f0;
      int b = (int)f1;
      if (a > b) return _true;
      return null;
    }

    //-<
    static Object ascii(Object[] f)
    {
      return _ascii(f[0]);
    }
    public static Object _ascii(Object f0)
    {
      return (Int32)((char)f0);
    }

    static Object mkchar(Object[] f)
    {
      return _mkchar(f[0]);
    }

    public static Object _mkchar(Object f0)
    {
      return (Char)((Int32)f0);
    }

    static Object stringescape(Object[] f)
    {
      string s = (string)f[0];
      string ret = "";
      foreach (char c in s)
        {
          if (c == '"') ret += "\\\"";
          else if (c == '\n') ret += @"\n";
          else if (c == '\t') ret += @"\t";
          else if (c == '\\') ret += @"\\";
          else ret += c;
        }
      return ret;
    }

    static Object hashput(Object[] f)
    {
      return _hashput(f[0], f[1], f[2]);
    }
    public static Object _hashput(Object f0, Object f1, Object f2)
    {
      Hashtable h = (Hashtable)f0;
      string key = f1.ToString();
      h[key] = f2;
      return f2;
    }

    static Object ohashput(Object[] f)
    {
      return _ohashput(f[0], f[1], f[2]);
    }
    public static Object _ohashput(Object f0, Object f1, Object f2)
    {
      Hashtable h = (Hashtable)f0;
      h[f1] = f2;
      return f2;
    }

    static Object hashget(Object[] f)
    {
      return _hashget(f[0], f[1]);
    }
    public static Object _hashget(Object f0, Object f1)
    {
      Hashtable h = (Hashtable)f0;
      string key = f1.ToString();
      return h[key];
    }

    static Object ohashget(Object[] f)
    {
      return _ohashget(f[0], f[1]);
    }
    public static Object _ohashget(Object f0, Object f1)
    {
      Hashtable h = (Hashtable)f0;
      return h[f1];
    }

    static Object shashput(Object[] f)
    {
      return _shashput(f[0], f[1], f[2]);
    }
    public static Object _shashput(Object f0, Object f1, Object f2)
    {
      System.Collections.Generic.Dictionary<Symbol,object>
        h = (System.Collections.Generic.Dictionary<Symbol,object>)f0;
      Symbol key = (Symbol)f1;
      h[key] = f2;
      return f2;
    }

    static Object shashget(Object[] f)
    {
      return _shashget(f[0], f[1]);
    }
    public static Object _shashget(Object f0, Object f1)
    {
      System.Collections.Generic.Dictionary<Symbol,object>
        h = (System.Collections.Generic.Dictionary<Symbol,object>)f0;
      object retval;
      Symbol key = (Symbol)f1;
      if(h.TryGetValue(key,out retval)) return retval;
      return null;
    }

    static Object mkhash(Object[] f)
    {
      return new System.Collections.Hashtable();
    }
    public static Object _mkhash()
    {
      return new System.Collections.Hashtable();
    }

    static Object mkshash(Object[] f)
    {
      return new System.Collections.Generic.Dictionary<Symbol, object>();
    }
    public static Object _mkshash()
    {
      return new System.Collections.Generic.Dictionary<Symbol, object>();
    }

    static Object stringappend(Object[] f)
    {
      string s = "";
      foreach (Object o in f)
        {
          s = s + o.ToString();
        }
      return s;
    }
    public static Object _stringappend(Object f0, Object f1)
    {
      if (f1 == null || f0 == null) return f0;
      string s = f0.ToString() + f1.ToString();

      return s;
    }
    static Object string2symbol(Object[] f)
    {
      return _string2symbol(f[0]);
    }
    public static Object _string2symbol(Object f0)
    {
      if (f0 == null) return null;
      string s = f0.ToString();
      return Symbol.make(s);
    }

    static Object any2string(Object[] f)
    {
      return _any2string(f[0]);
    }
    public static Object _any2string(Object f0)
    {
      if (f0 == null) return "";
      Object s = f0;
      return s.ToString();
    }

    static Object booleanp(Object[] f)
    {
      return _booleanp(f[0]);
    }
    public static Object _booleanp(Object f0)
    {
      if (f0 == null) return _true;
      if (f0 is bool) return _true;
      return null;
    }
    static Object numberp(Object[] f)
    {
      return _numberp(f[0]);
    }
    public static Object _numberp(Object f0)
    {
      if (f0 == null) return null;
      if (f0 is int) return _true;
      return null;
    }

    static Object symbolp(Object[] f)
    {
      return _symbolp(f[0]);
    }
    public static Object _symbolp(Object f0)
    {
      if (f0 == null) return null;
      if (f0 is Symbol) return _true;
      return null;
    }
    static Object charp(Object[] f)
    {
      return _charp(f[0]);
    }
    public static Object _charp(Object f0)
    {
      if (f0 == null) return null;
      if (f0 is Char) return _true;
      return null;
    }

    static Object stringp(Object[] f)
    {
      return _stringp(f[0]);
    }
    public static Object _stringp(Object f0)
    {
      if (f0 == null) return null;
      if (f0 is string) return _true;
      return null;
    }

    static Object getmacroenv(Object[] f)
    {
      return Precompiler.macros;
    }

    static Object getfuncenv(Object[] f)
    {
      return Precompiler.symbols;
    }

    static Object defmacro(Object[] f)
    {
      Symbol nm = f[0] as Symbol;
      if (nm == null)
        {
          Console.WriteLine("DEFMACRO ERROR: " + f[0].ToString());
          nm = Symbol.make(f[0].ToString());
        }
      Precompiler.macros[nm] = f[1];
      return null;
    }
    public static Object _ccdefmacro(Object nm, Object vl)
    {
      Symbol snm = nm as Symbol;
      if (snm == null)
        {
          Console.WriteLine("CCDEFMACRO ERROR: " + nm.ToString());
          snm = Symbol.make(nm.ToString());
        }
      Precompiler.macros[snm] = vl;
      return null;
    }
    public static Object __ccdef(Object[] o)
    {
      return _ccdef(o[1], o[2]);
    }
    public static Object _ccdef(Object nm, Object vl)
    {
      Symbol snm = (Symbol)nm;
      Precompiler.symbols[snm] = vl;
      return null;
    }

    public static Object _add(Object f0, Object f1)
    {
      return (Int32)((Int32)f0 + (Int32)f1);
    }
    public static Object _sub(Object f0, Object f1)
    {
      return (Int32)((Int32)f0 - (Int32)f1);
    }
    public static Object _mul(Object f0, Object f1)
    {
      return (Int32)((Int32)f0 * (Int32)f1);
    }
    public static Object _div(Object f0, Object f1)
    {
      return (Int32)((Int32)f0 / (Int32)f1);
    }
    static Object add(Object[] f)
    {
      return (Int32)((Int32)f[0] + (Int32)f[1]);
    }
    static Object sub(Object[] f)
    {
      return (Int32)((Int32)f[0] - (Int32)f[1]);
    }

    static Object mul(Object[] f)
    {
      return (Int32)((Int32)f[0] * (Int32)f[1]);
    }
    static Object div(Object[] f)
    {
      return (Int32)((Int32)f[0] / (Int32)f[1]);
    }
    static Object mod(Object[] f)
    {
      return (Int32)((Int32)f[0] % (Int32)f[1]);
    }

    static Object bitand(Object[] f)
    {
      return (Int32)((Int32)f[0] & (Int32)f[1]);
    }
    static Object bitor(Object[] f)
    {
      return (Int32)((Int32)f[0] | (Int32)f[1]);
    }
    public static Object _bitand(Object f0, Object f1)
    {
      return (Int32)((Int32)f0 & (Int32)f1);
    }
    public static Object _bitor(Object f0, Object f1)
    {
      return (Int32)((Int32)f0 | (Int32)f1);
    }
    static Object cons(Object[] f)
    {
      return new Pair(f[0], f[1]);
    }
    public static Object _cons(Object f0, Object f1)
    {
      return new Pair(f0, f1);
    }
    static Object car(Object[] f)
    {
      return _car(f[0]);
    }
    public static Object _car(Object f0)
    {
      if (f0 == null)
        {
          Console.WriteLine("CAR:::NULL");
          throw new Exception();
        }
      if (f0 is Pair) return ((Pair)f0).car;
      Console.WriteLine("CAR:::" + f0 + " of " + f0.GetType());
      return null;
    }
    static Object cdr(Object[] f)
    {
      return _cdr(f[0]);
    }
    public static Object _cdr(Object f0)
    {
      if (f0 == null)
        {
          Console.WriteLine("CDR:::NULL");
          throw new Exception();
        }
      if (f0 is Pair) return ((Pair)f0).cdr;

      Console.WriteLine("CDR:::" + f0 + " of " + f0.GetType());
      return null;
    }
    static Object nullp(Object[] f)
    {
      return _nullp(f[0]);
    }
    public static Object _nullp(Object f0)
    {
      if (f0 == null) return _true;
      return null;
    }
    static Object pairp(Object[] f)
    {
      return _Pairp(f[0]);
    }
    public static Object _Pairp(Object f0)
    {
      if (f0 == null) return null;
      if (f0 is Pair) return _true;
      return null;
    }
    static Object def(Object[] f)
    {
      return _def(f[0], f[1]);
    }
    public static Object _def(Object f0, Object f1)
    {
      Symbol nm;
      if(f0 is Symbol) {
        nm = (Symbol)f0;
      } else nm = Symbol.make((String)f0);
      Precompiler.symbols[nm] = f1;
      return null;
    }
    static Object eval(Object[] f)
    {
      Object p = f[0];
      Code c = Precompiler.compile(p);
      return c.run(f, null);
    }


    static Object string_to_list(Object[] f)
    {
      return _string_to_list(f[0]);
    }

    public static Object _string_to_list(Object f0)
    {
      string str = (String)f0;
      int len = str.Length;
      if (len < 1) return null;
      int i = 0;
      Pair top = new Pair(null, null);
      Pair cur = top;
      for (i = 0; i < len; i++)
        {
          if (i > 0) { Pair np = new Pair(null, null); cur.cdr = np; cur = np; }
          cur.car = (Object)(str[i]);
        }
      return (Object)top;
    }

    static Object list_to_string(Object[] f)
    {
      return _list_to_string(f[0]);
    }
    public static Object _list_to_string(Object f0)
    {
      if (f0 == null) return "";
      Pair p = (Pair)f0;
      int len = p.length();
      char[] ch = new char[len];
      for (int i = 0; i < len; i++)
        {
          ch[i] = (char)(p.car); p = (Pair)p.cdr;
        }
      return new String(ch);
    }
#endregion
#region Compiler support
    public static Hashtable comp_hash = new Hashtable();
    public static Hashtable comp_hash_f = new Hashtable();
    public static Hashtable comp_hash_nargs = new Hashtable();

    public static Pair deplist = null;

    public static bool compmode = true;

    public static Object _get_comp_hashes()
    {
      return new Pair(comp_hash,
                      new Pair(comp_hash_f,
                               new Pair(comp_hash_nargs,
                                        new Pair(nativs, null))));
    }

    public static void _add_dep(System.RuntimeMethodHandle hndl)
    {
      deplist = new Pair((object)System.Reflection.MethodBase.GetMethodFromHandle(hndl), (object)deplist);
    }

    public static Object _get_dep()
    {
      return deplist;
    }

    public static void _clean_dep()
    {
      deplist = null;
    }

    public static void _register_method(Symbol globname, System.RuntimeMethodHandle hndl, int nargs)
    {
      string ntmp = globname.v;
      comp_hash[ntmp] = System.Reflection.MethodBase.GetMethodFromHandle(hndl);
      comp_hash_nargs[ntmp] = (Object)nargs;
      comp_hash_f[ntmp] = null;
      Precompiler.symbols[globname] = makedelegate(globname);
    }

    public static void _register_field(Symbol globname, System.RuntimeFieldHandle hndl, Object value)
    {
      comp_hash_f[globname.v] = System.Reflection.FieldInfo.GetFieldFromHandle(hndl);
      comp_hash[globname.v] = null;
      Precompiler.symbols[globname] = value;
    }

    public static void _register_macro(String name, Object value)
    {
      Precompiler.macros[Symbol.make(name)] = value;
    }

    public static object makedelegate(Symbol name)
    {
      return makedelegate(name.v);
    }

    public static object makedelegate(String name)
    {
      MethodBase v = (MethodBase)(comp_hash[name]);
      object nargs = (comp_hash_nargs[name]);
      SCWrapper wraper = new SCWrapper();
      wraper.mtdi = v;
      return new SimpleCode(wraper.run);
    }

#endregion


  }
  public class SCWrapper
  {
    public MethodBase mtdi;
    public object run(Object[] args)
    {
      try
        {
          return mtdi.Invoke(null, args);
        }
      catch (System.Reflection.TargetInvocationException e)
        {
          throw e.InnerException;
        }
    }
  }

  public class DebugHelper
  {
    public static void 
    setDebuggable(System.Reflection.Emit.AssemblyBuilder assemblyBuilder)
    {
      Type daType = typeof(DebuggableAttribute);
      ConstructorInfo daCtor = 
        daType.GetConstructor(new Type[] {
            typeof(DebuggableAttribute.DebuggingModes) });
      CustomAttributeBuilder daBuilder = 
        new CustomAttributeBuilder(daCtor, new object[] {
            DebuggableAttribute.DebuggingModes.DisableOptimizations |
            DebuggableAttribute.DebuggingModes.Default });
      assemblyBuilder.SetCustomAttribute(daBuilder);
    }
  }
}
