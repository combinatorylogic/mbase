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
public delegate Object AltFun0();
public interface AltClosure0 {
  Object run();}
public delegate Object AltFun1(Object a0);
public interface AltClosure1 {
  Object run(Object a0);}
public delegate Object AltFun2(Object a0,Object a1);
public interface AltClosure2 {
  Object run(Object a0,Object a1);}
public delegate Object AltFun3(Object a0,Object a1,Object a2);
public interface AltClosure3 {
  Object run(Object a0,Object a1,Object a2);}
public delegate Object AltFun4(Object a0,Object a1,Object a2,Object a3);
public interface AltClosure4 {
  Object run(Object a0,Object a1,Object a2,Object a3);}
public delegate Object AltFun5(Object a0,Object a1,Object a2,Object a3,Object a4);
public interface AltClosure5 {
  Object run(Object a0,Object a1,Object a2,Object a3,Object a4);}
public delegate Object AltFun6(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5);
public interface AltClosure6 {
  Object run(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5);}
public delegate Object AltFun7(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6);
public interface AltClosure7 {
  Object run(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6);}
public delegate Object AltFun8(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7);
public interface AltClosure8 {
  Object run(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7);}
public delegate Object AltFun9(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7,Object a8);
public interface AltClosure9 {
  Object run(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7,Object a8);}
public delegate Object AltFun10(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7,Object a8,Object a9);
public interface AltClosure10 {
  Object run(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7,Object a8,Object a9);}
public delegate Object AltFun11(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7,Object a8,Object a9,Object a10);
public interface AltClosure11 {
  Object run(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7,Object a8,Object a9,Object a10);}
public delegate Object AltFun12(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7,Object a8,Object a9,Object a10,Object a11);
public interface AltClosure12 {
  Object run(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7,Object a8,Object a9,Object a10,Object a11);}
public delegate Object AltFun13(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7,Object a8,Object a9,Object a10,Object a11,Object a12);
public interface AltClosure13 {
  Object run(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7,Object a8,Object a9,Object a10,Object a11,Object a12);}
public delegate Object AltFun14(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7,Object a8,Object a9,Object a10,Object a11,Object a12,Object a13);
public interface AltClosure14 {
  Object run(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7,Object a8,Object a9,Object a10,Object a11,Object a12,Object a13);}
public delegate Object AltFun15(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7,Object a8,Object a9,Object a10,Object a11,Object a12,Object a13,Object a14);
public interface AltClosure15 {
  Object run(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7,Object a8,Object a9,Object a10,Object a11,Object a12,Object a13,Object a14);}
public delegate Object AltFun16(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7,Object a8,Object a9,Object a10,Object a11,Object a12,Object a13,Object a14,Object a15);
public interface AltClosure16 {
  Object run(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7,Object a8,Object a9,Object a10,Object a11,Object a12,Object a13,Object a14,Object a15);}
public delegate Object AltFun17(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7,Object a8,Object a9,Object a10,Object a11,Object a12,Object a13,Object a14,Object a15,Object a16);
public interface AltClosure17 {
  Object run(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7,Object a8,Object a9,Object a10,Object a11,Object a12,Object a13,Object a14,Object a15,Object a16);}
public delegate Object AltFun18(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7,Object a8,Object a9,Object a10,Object a11,Object a12,Object a13,Object a14,Object a15,Object a16,Object a17);
public interface AltClosure18 {
  Object run(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7,Object a8,Object a9,Object a10,Object a11,Object a12,Object a13,Object a14,Object a15,Object a16,Object a17);}
public delegate Object AltFun19(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7,Object a8,Object a9,Object a10,Object a11,Object a12,Object a13,Object a14,Object a15,Object a16,Object a17,Object a18);
public interface AltClosure19 {
  Object run(Object a0,Object a1,Object a2,Object a3,Object a4,Object a5,Object a6,Object a7,Object a8,Object a9,Object a10,Object a11,Object a12,Object a13,Object a14,Object a15,Object a16,Object a17,Object a18);}
}
