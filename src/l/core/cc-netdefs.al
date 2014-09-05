;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;{{
(define t_ConstructorInfo (sdotnet "System.Reflection.ConstructorInfo"))
(define mtd_MkSymbol (r_mtd t_Symbol "make" string))
(define mtd_Cons (r_getconstructor t_Pair object object))
(net.types RuntimeMethodHandle RuntimeFieldHandle Precompiler Runtime)

(define mtd_register_method (r_mtd t_runtime "_register_method" t_Symbol t_RuntimeMethodHandle int))
(define mtd_register_field (r_mtd t_runtime "_register_field" 
					   t_Symbol t_RuntimeFieldHandle t_object))

(define mtd_register_macro (r_mtd t_runtime "_register_macro" 
					   t_string t_object))

(define fld_True (r_getField t_runtime "_true"))
(define fld_False (r_getField t_runtime "_false"))

(define f_Assmblys (r_getField t_runtime "assmblys"))

(define f_Pc_symbols (r_getField t_Precompiler "symbols"))
(define fld_CompMode (r_getField t_runtime "compmode"))
(define m_getSItem  (r_mtd (r_GetType (getfuncenv)) "get_Item" t_Symbol))
(define m_getItem  (r_mtd "System.Collections.Hashtable" "get_Item" t_object))
(define m_hashSetItem  (r_mtd "System.Collections.Hashtable" "set_Item" t_object t_object))
(define m_hashAdd  (r_mtd "System.Collections.Hashtable" "Add" t_object t_object))
(define t_Hashtable (r_typebyname "System.Collections.Hashtable"))
(define ctr_Hashtable  (r_getconstructor t_Hashtable))

(define m_WriteLine (r_mtd "System.Console" "WriteLine" t_object))

(define m_typefromhandle  (r_mtd "System.Type" "GetTypeFromHandle" "System.RuntimeTypeHandle"))

(define m_getassembly  (r_mtd "System.Type" "get_Assembly"))
(define m_getassemblyname  (r_mtd "System.Reflection.Assembly" "GetName"))
(define m_getassemblyshortname  (r_mtd "System.Reflection.AssemblyName" "get_Name"))

(define f_Cdr  (r_getField t_Pair "cdr"))

(define cc:boot:getenv (r_tsbind t_runtime "_get_comp_hashes"))

(define get_BaseType (r_tbind t_type "get_BaseType"))

(define t_AltClosure (dotnet "AltClosure"))
(define r_metod (r_tbind t_Type "GetMethod" string))
(define r_iclass (r_tbind t_Type "GetNestedType" string))
(define mtd_Add_Dependency (r_mtd t_Runtime "_add_dep" t_RuntimeMethodHandle))

(define _Get_Deps (r_tsbind t_Runtime "_get_dep"))
(define _Clean_Deps (r_tsbind t_Runtime "_clean_dep"))
(define m_Runtime_setargs  (r_mtd t_Runtime "setargs" "System.String[]"))
(define m_Runtime_init (r_mtd t_Runtime "iRuntime"))
(define t_string_array (r_typebyname "System.String[]"))
(define __get_Parameters (r_tbind (dotnet "System.Reflection.MethodBase")
                                  "GetParameters"))

(define mtd_hash (r_mtd t_Symbol "hash" int))
(define t_ByteArray (r_typebyname "System.Byte[]"))
(function vector? (v) (t_ass? t_ByteArray (r_GetType v)))
(define mtd_InitializeArray (r_mtd "System.Runtime.CompilerServices.RuntimeHelpers" "InitializeArray" "System.Array" "System.RuntimeFieldHandle"))

;}}
