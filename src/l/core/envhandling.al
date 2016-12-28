;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(macro env:makenew ()
  '(mkhash))

(macro env:getpath_: (env path)
  `(hashget ,env ,path))

(macro env:setpath_: (env path vlu)
  `(hashput ,env ,path ,vlu))

(function _env:makepath_: (pth)
  (p:match pth
    ($$M:v (S<< v))
    (($$M:i . $rest)
     (to-string (cons (S<< i) rest)))))

(macro env:makepath_: (pth)
  (p:match pth
    ($$M:v (S<< v))
    (($$M:i . $rest)
     `(to-string (list ,(S<< i) ,@rest)))))

(macro env:unroll_: envpath
    (p:match envpath
      ($$M:m m)
      (($hd . $tl)
       (let loop ((e envpath))
         (p:match e
           (($$M:a $$M:b) `(env:getpath_: ,a ,(_env:makepath_: b)))
           (($hd . $tl)
            `(env:getpath_: ,(loop (cuttail e))
                            ,(env:makepath_: ,(car (lasttail e))))))))))

(macro env:update_: (env val)
  (p:match val
    ((:gen $lst)
     (with-syms (ss sd)
       `(let ((,ss ,lst))
          (foreach (,sd ,ss)
            (env:setpath_: ,env (_env:makepath_: (car ,sd)) (cadr ,sd))))))
    (($p $rest) `(env:setpath_: ,env (env:makepath_: ,p) ,rest))))

(macro env:check: (e . body)
  (format e (env path)
    (with-syms (s pth xx ee)
       `(let* ((,ee (env:unroll_: ,@env))
               (,pth (env:makepath_: ,path))
               (,s  (env:getpath_: ,ee ,pth)))
          (if ,s ,s
              (let ((,xx (begin ,@body)))
                (env:setpath_: ,ee ,pth ,xx)
                ,xx))))))

(macro env:set: (env . vals)
  (with-syms (e)
   `(let ((,e (env:unroll_: ,@env)))
        ,@(foreach-map (v vals)
            `(env:update_: ,e ,v))
        ,e
        )))

(macro env:get: (env path)
  (with-syms (e)
    `(let ((,e (env:unroll_: ,@env)))
       (env:getpath_: ,e (env:makepath_: ,path)))))

(macro env:new: vals
  (with-syms (e)
    `(let ((,e (env:makenew)))
       ,@(foreach-map (v vals)
            `(env:update_: ,e ,v))
       ,e
       )))

(macro env:collect: (env path val)
  (with-syms (e pth)
    `(let* ((,e (env:unroll_: ,@env))
            (,pth (env:makepath_: ,path)))
       (env:setpath_: ,e ,pth (noconst (cons ,val (env:getpath_: ,e ,pth)))))))

(macro env:inc: (env name)
  (with-syms (s1 s2)
   `(let* ((,s1 (env:get: ,env ,name))
           (,s2 (if ,s1 (+ ,s1 1) 0)))
      (env:set: ,env (,name ,s2))
      ,s2)))




