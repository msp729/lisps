(set wholep
     '(lambda x.
        (cond
          ((atom x) . nil)
          ((atom (cdr x))
           . (cond
               ((eq (cdr x) 0) . (wholep (car x)))
               ((eq (cdr x) 1) . (natp (car x)))
               (t . nil)))
          (t . nil))))

(set natp '(lambda x. (cond
                        ((atom x) . (cond
                                    ((eq x 0) . t)
                                    (t . nil)))
                        (t . (wholep x)))))

(set ite '(lambda (bool then else). (cond (bool . then) (t . else))))
(set not '(lambda x. (ite x nil t)))

(set wholepat
     '(lambda (2x 2x+1 w).
        (cond
          ((eq (cdr w) 0). (2x (car w)))
          ((eq (cdr w) 1). (2x+1 (car w))))))

(set natpat
     '(lambda (zero whole n).
        (cond
          ((eq n 0). zero)
          (t. (whole n)))))

(set wholerec
     '(lambda (2x 2x+1 zero w).
        (wholepat
          '(lambda w. (2x (wholerec 2x 2x+1 zero w)))
          '(lambda w. (2x+1 (natrec 2x 2x+1 zero w)))
          w)))

(set natrec
     '(lambda (2x 2x+1 zero n).
        (natpat
          zero
          '(lambda w. (wholerec 2x 2x+1 zero w))
          w)))

(set inc '(lambda n. (natpat '(0 . 1)
                             '(lambda w. (wholepat
                                           '(lambda half. (cons half 1))
                                           '(lambda halfless. (cons (inc halfless) 0))
                                           w))
                             n)))

(set + '(lambda (a b). (natpat b
       '(lambda a. (natpat a
          '(lambda b.
             (wholepat
               '(lambda a/2. (wholepat
                  '(lambda b/2. (cons (+ a/2 b/2) 0))
                  '(lambda b-1/2. (cons (+ a/2 b-1/2) 1))
                  b))
               '(lambda a-1/2. (wholepat
                  '(lambda b/2. (cons (+ a-1/2 b/2) 1))
                  '(lambda b-1/2. (cons (inc (+ a-1/2 b-1/2)) 0))
                  b))
               a))
          b))
       a)))

(set 2x '(lambda x ite (eq x 0) 0 (cons x 0)))
