//////////////////////////////////////////////////////////////////////////////
//
//   OpenMBase
//
// Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
//
//
//////////////////////////////////////////////////////////////////////////////

using System;
using System.Collections;
using System.IO;
using System.Reflection;


namespace Meta.Scripting
{
  class CommandLine
  {
    static void ShowHelp()
    {

      Console.WriteLine(
                        "MBase (c) by Meta Alternative Ltd., 2005-2017\n" +
                        "Usage:\n" +
                        "mb0 [opts] /BOOT bootfile.al\n" +
                        "mb0 [opts] /C file.al file.alc : compile to layer0\n" +
                        "mb0 [opts] - : REPL\n" +
                        "mb0 [opts] file.al : execute script\n" +
                        "where [opts] are:\n"+
                        " /I file.alc : use a given alc file for initialisation " +
                        " /D file.dll : use a given dll for initialisation "
                        );
    }

    public static void bootfile(string nm, string on)
    {
      ExtendedReader tr = new ExtendedReader(new StreamReader(nm));
      Runtime.xpath = Path.GetDirectoryName(nm);
      StreamWriter wr = new StreamWriter(on);
      while (true)
        {
          Object o = Runtime.aread(tr);
          if (o == null)
            {
              wr.Close();
              tr.Close();
              return;
            }

          Object result =
            Runtime.apply(Symbol.make("read-compile-eval-dump"),
                          new Object[] { o });
          if (result != null) wr.WriteLine(result);
        }
    }

    static int apos = 0;
    static string[] sargs;

    static void addinit_al(string fname)
    {
      Runtime.readfile(fname);
    }

    static void addinit_dll(string fname)
    {
      Runtime.rundll(fname);
    }

    static bool consumearg()
    {
      if (apos >= sargs.Length) return false;
      string a0 = sargs[apos].ToUpper();
      if (a0 == "/P")
        {
          Console.ReadLine();
          apos ++;

        } else if (a0 == "/I")
            {
              addinit_al(sargs[apos + 1]);
              apos += 2;
            }
      else if (a0 == "/D")
        {
          addinit_dll(sargs[apos + 1]);
          apos += 2;
        }
      else if (a0 == "-")
        {
          Runtime.setargs(arest(sargs, apos + 1));
          StreamReader t = new StreamReader(Console.OpenStandardInput());

          Runtime.runfile((StreamReader)t, true);
          return false;
        }
      else if (a0 == "/C")
        {
          Runtime.setargs(arest(sargs, apos + 3));
          check(sargs, apos, 3);
          bootfile(sargs[apos + 1], sargs[apos + 2]);
          return false;
        }
      else if (a0 == "/BOOT")
        {
          string epth = Assembly.GetExecutingAssembly().Location;
          string pth = Path.GetDirectoryName(epth);
          string boot = Path.Combine(pth, "boot.alc");
          string bsrc = sargs[apos + 1];
          Console.WriteLine("Building bootstrap compiler");
          Precompiler.macros.Clear();
          bootfile(bsrc, boot);
          return false;
        }
      else
        {
          Runtime.setargs(arest(sargs, apos + 1));
          Runtime.runfile(sargs[apos]);
          return false;
        }
      return true;
    }

    static void check(string[] args, int l)
    {
      if (args.Length < l) { ShowHelp(); }
    }
    static void check(string[] args, int pos, int l)
    {
      if (args.Length < pos + l) { ShowHelp(); }
    }
    static string[] arest(string[] args, int f)
    {
      int l = args.Length;
      string[] x = new string[l - f];
      for (int i = f; i < l; i++) x[i - f] = args[i];
      return x;
    }

    static Runtime rt = null;

    [STAThread]
    static void Main(string[] args)
    {
      try
        {
          Runtime.iRuntime();

          if (args.Length == 0) { ShowHelp(); return; }
          sargs = args; apos = 0;
          while (consumearg()) ;

        }
      catch (Exception e)
        {
          Console.WriteLine("Unhandled exception in the top level REPL: " + e.ToString());
          if (e is MBaseException && rt != null)
            {
              Console.WriteLine("\n\nMBaseException: " + Runtime.print(((MBaseException)e).val()));
            }
          Environment.Exit(-1);
        }
    }
  }
}
