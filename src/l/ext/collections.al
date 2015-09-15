;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; An interface to some of the .NET collection datatypes.

(Section "{\\tt System.Collections} bindings")

(using ("System.Collections")

  (net.types Stack Queue ArrayList ICollection IEnumerable IEnumerator)
)

;; 1. System.Collections.Stack

(function stack:new ()
  "New [Stack] instance"
  ((r_constr t_Stack)))

(function stack:push (s o)
  "Push an object on the [Stack]"
  ((r_tbind t_Stack "Push" t_object) s o))

(function stack:peek (s)
  "Peek an object on top of [Stack]"
  ((r_tbind t_Stack "Peek") s))

(function stack:pop (s)
  "Pop an object from [Stack]"
  ((r_tbind t_Stack "Pop") s))

(function stack:count (s)
  "Number of elements in [Stack]"
  ((r_tbind t_Stack "get_Count") s))


 ;; 2. ArrayList.

(function alist:new () ;; simplest constructor
  "New [ArrayList] instance"
  ((r_constr t_ArrayList)))

(function alist:new:n (n)
  "New [ArrayList] instance, [n] storage space preallocated"
  ((r_constr t_ArrayList int) n))

(function alist:new:l (l)
  "New [ArrayList] instance, initialized from the list [l]"
  ((r_constr t_ArrayList t_ICollection) l))


(function alist:add (al v)
  "Add an element to the end of [ArrayList]"
  ((r_tbind t_ArrayList "Add" object) al v))

(function alist:length (al)
  "Number of elements in an [ArrayList]"
  ((r_tbind t_ArrayList "get_Count") al))

(function alist:get (al n)
  "Get a numbered element of an [ArrayList]"
  ((r_tbind t_ArrayList "get_Item" int) al n))

(function alist:set (al n v)
  "Set (destructively!) a value of a numbered [ArrayList] element"
  ((r_tbind t_ArrayList "set_Item" int object) al n v))

(function alist->a (al)
  "Convert an [ArrayList] to [object] array."
  ((r_tbind t_ArrayList "ToArray") al))

(function alist->l (al)
  "Convert an [ArrayList] to list"
  (a->l (alist->a al)))


;;; System.Collections.Queue


(function queue:new ()
  "New [Queue] instance"
  ((r_constr t_Queue)))

(function queue:new:n (n)
  "New [Queue] instance, [n] storage space preallocated"
  ((r_constr t_Queue int) n))

(function queue:new:l (l)
  "New [Queue] instance, initialized from the list [l]"
  ((r_constr t_Queue t_ICollection) l))


(function queue:length (q)
  "Number of elements in a [Queue]"
  ((r_tbind t_Queue "get_Count") q))

(function queue:add (q v)
  "Add an element to the [Queue]"
  ((r_tbind t_Queue "Enqueue" object) q v)
  )

(function queue:get (q)
  "Get an element of the [Queue]"
  ((r_tbind t_Queue "Dequeue") q))

(function queue:peek (q)
  "Peek an object of [Queue]"
  ((r_tbind t_Queue "Peek") q))

(function queue->a (q)
  "Convert a [Queue] to [object] array."
  ((r_tbind t_Queue "ToArray") q))

(function queue->l (q)
  "Convert a [Queue] to list"
  (a->l (queue->a q)))

;;; Generic iterators (duplicating some bits of netlib functionality)

(function ienum:current (en)
  ((r_tbind t_IEnumerator "get_Current") en))

(function ienum:next (en)
  ((r_tbind t_IEnumerator "MoveNext") en))

(function ienum:make (enb)
  ((r_tbind t_IEnumerable "GetEnumerator") enb))

(function ienumb:iter (enb f)
  (let ((en (ienum:make enb)))
     (whiledo
       (ienum:next en)
       (f (ienum:current en)))))

(function ienumb:map (enb f)
  (let ((en (ienum:make enb)))
     (whilemap
       (ienum:next en)
       (f (ienum:current en)))))

(macro n.foreach (h . body)
  ("Usage:"
   "[["
   "(n.foreach (<name> <IEnumerable>) <expr>*)"
   "]]"
   )
  (format h (nm vl)
    `(ienumb:iter ,vl (fun (,nm) ,@body))))

(macro n.foreach-map (h . body)
  ("Usage:"
   "[["
   "(n.foreach-map (<name> <IEnumerable>) <expr>*)"
   "]]"
   )
  (format h (nm vl)
     `(ienumb:map ,vl (fun (,nm) ,@body))))



(macro with-queue (aaa . body)
  (format aaa (addname . gets)
   (with-syms (que)
    `(let* ((,que (queue:new))
            (,addname (cut queue:add ,que <>))
            ,@(if (null? (car gets)) nil
                  `((,(car gets) (fun () (queue:get ,que)))))
            ,@(if (null? (cdr gets)) nil
                  `((,(cadr gets)
                     (fun () (let loop ()
                               (if (< (queue:length ,que) 1) nil
                                   (cons (queue:get ,que) (loop)))))))))
       ,@body))))

