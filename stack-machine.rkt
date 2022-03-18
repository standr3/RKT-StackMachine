#lang racket
(require "opcodes.rkt")
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)


;; TODO 1:
;; Alegeți metoda de reprezentarea a unei stive.
;; Implementați:
(define empty-stack null)
(define (make-stack) '())

(define (push element stack)
  (if (equal? stack 'None)
      (cons element '())
      (cons element stack)))
(define (top stack)
  (if (equal? stack 'None)
      '()
      (car stack)))
(define (pop stack)
  (if (equal? stack 'None)
      'None
      (cdr stack)))

;; TODO 2:
;; Alegeți metoda de reprezentare a unei mașini stivă.
;; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
;; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
;; la ce instrucțiune sunteți.
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (hash
   'STACK stack
   'CO-VARNAMES co-varnames
   'CO-CONSTS co-consts
   'CO-NAMES co-names
   'CO-CODE co-code
   'INSTRUCTION-COUNTER IC))

;; Definiți funcțiile `get-varnames`, `get-consts`, `get-names`,
;; `get-code`, `get-stack`, `get-IC` care primesc o mașina stivă și întorc
;; componenta respectivă

;; ex:
;; > (get-varnames (make-stack-machine empty-stack 'dummy-co-varnames (hash) (hash) (list) 0))
;; 'dummy-co-varnames
(define (get-varnames stack-machine)
  (hash-ref stack-machine 'CO-VARNAMES))

;; ex:
;; > (get-consts (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0))
;; 'dummy-co-consts
(define (get-consts stack-machine)
  (hash-ref stack-machine 'CO-CONSTS))

;; ex:
;; > (get-names (make-stack-machine empty-stack (hash) (hash) 'dummy-co-names (list) 0))
;; 'dummy-co-names
(define (get-names stack-machine)
  (hash-ref stack-machine 'CO-NAMES))

;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) 'dummy-co-code 0))
;; dummy-co-code
(define (get-code stack-machine)
  (hash-ref stack-machine 'CO-CODE))

;; Întoarce stiva de execuție.
;; ex:
;; > (get-code (make-stack-machine 'dummy-exec-stack (hash) (hash) (hash) (list) 0))
;; dummy-exec-stack
(define (get-stack stack-machine)
  (hash-ref stack-machine 'STACK))

;; Întoarce instruction counterul.
;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) (list) 0))
;; 0
(define (get-IC stack-machine)
  (hash-ref stack-machine 'INSTRUCTION-COUNTER))



(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

;; TODO 3:
;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.
(define (get-symbol-index symbol)
  (index-of symbols symbol))

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES stack-machine))
;; "new-varnames"
;; > (get-varnames (update-stack-machine "new-names" 'CO-NAMES stack-machine))
;; "new-names"
(define (update-stack-machine item symbol stack-machine)
  (hash-set stack-machine symbol item))

;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție
(define (push-exec-stack value stack-machine)
  (hash-set stack-machine 'STACK (push value (get-stack stack-machine))))

;;  Definiți funcția pop-exec-stack care primește o masină stivă
;;  și intoarce o noua mașina aplicând pop pe stiva de execuție.
(define (pop-exec-stack stack-machine)
  (hash-set stack-machine 'STACK (pop (get-stack stack-machine))))

;; TODO 4:
;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.
(define (run-stack-machine stack-machine)
  (if (or (equal? (get-IC stack-machine) (- (length (get-code stack-machine)) 1))
          (equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'RETURN_VALUE))
              stack-machine
              (cond
                ;; LOAD_CONST(const_i)
                ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'LOAD_CONST)
                         (run-stack-machine
                          (push-exec-stack
                           (hash-ref (get-consts stack-machine) (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))))
                           (update-stack-machine
                            (+ (get-IC stack-machine) 1)
                            'INSTRUCTION-COUNTER
                            stack-machine))))
                ;; LOAD_FAST(var_i)
                ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'LOAD_FAST)
                         (run-stack-machine
                          (push-exec-stack
                           (hash-ref (get-varnames stack-machine) (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))))
                           (update-stack-machine
                            (+ (get-IC stack-machine) 1)
                            'INSTRUCTION-COUNTER
                            stack-machine))))
                ;; STORE_FAST(var_i)
                ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'STORE_FAST)
                         (run-stack-machine
                          (pop-exec-stack
                           (update-stack-machine
                            (hash-set (get-varnames stack-machine) (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))) (top (get-stack stack-machine)))
                            'CO-VARNAMES
                            (update-stack-machine
                             (+ (get-IC stack-machine) 1)
                             'INSTRUCTION-COUNTER
                             stack-machine)))))
                ;; BINARY_MODULO
                ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'BINARY_MODULO)
                         (run-stack-machine
                          (push-exec-stack
                           (modulo (top (cdr (get-stack stack-machine))) (top (get-stack stack-machine)))
                           (pop-exec-stack
                            (pop-exec-stack
                             (update-stack-machine
                              (+ (get-IC stack-machine) 1)
                              'INSTRUCTION-COUNTER
                              stack-machine))))))
                ;; BINARY_ADD
                ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'BINARY_ADD)
                         (run-stack-machine
                          (push-exec-stack
                           (+ (top (cdr (get-stack stack-machine))) (top (get-stack stack-machine)))
                           (pop-exec-stack
                            (pop-exec-stack
                             (update-stack-machine
                              (+ (get-IC stack-machine) 1)
                              'INSTRUCTION-COUNTER
                              stack-machine))))))
                ;; BINARY_SUBTRACT
                ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'BINARY_SUBTRACT)
                         (run-stack-machine
                          (push-exec-stack
                           (- (top (cdr (get-stack stack-machine))) (top (get-stack stack-machine)))
                           (pop-exec-stack
                            (pop-exec-stack
                             (update-stack-machine
                              (+ (get-IC stack-machine) 1)
                              'INSTRUCTION-COUNTER
                              stack-machine))))))
                ;; INPLACE_ADD
                ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'INPLACE_ADD)
                         (run-stack-machine
                          (push-exec-stack
                           (+ (top (cdr (get-stack stack-machine))) (top (get-stack stack-machine)))
                           (pop-exec-stack
                            (pop-exec-stack
                             (update-stack-machine
                              (+ (get-IC stack-machine) 1)
                              'INSTRUCTION-COUNTER
                              stack-machine))))))
                ;; INPLACE_SUBTRACT
                ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'INPLACE_SUBTRACT)
                         (run-stack-machine
                          (push-exec-stack
                           (- (top (cdr (get-stack stack-machine))) (top (get-stack stack-machine)))
                           (pop-exec-stack
                            (pop-exec-stack
                             (update-stack-machine
                              (+ (get-IC stack-machine) 1)
                              'INSTRUCTION-COUNTER
                              stack-machine))))))
                ;; INPLACE_MODULO
                ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'INPLACE_MODULO)
                         (run-stack-machine
                          (push-exec-stack
                           (modulo (top (cdr (get-stack stack-machine))) (top (get-stack stack-machine)))
                           (pop-exec-stack
                            (pop-exec-stack
                             (update-stack-machine
                              (+ (get-IC stack-machine) 1)
                              'INSTRUCTION-COUNTER
                              stack-machine))))))
                ;; LOAD_GLOBAL(func_i)
                ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'LOAD_GLOBAL)
                         (run-stack-machine
                          (push-exec-stack
                           (hash-ref (get-names stack-machine) (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))))
                           (update-stack-machine
                            (+ (get-IC stack-machine) 1)
                            'INSTRUCTION-COUNTER
                            stack-machine))))
                ;; COMPARE_OP(i)
                ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'COMPARE_OP)
                         (run-stack-machine
                          (push-exec-stack
                           (if ((get-cmpop (cdr (list-ref (get-code stack-machine) (get-IC stack-machine)))) (top (cdr (get-stack stack-machine))) (top (get-stack stack-machine)))
                               #t
                               #f)
                           (pop-exec-stack
                            (pop-exec-stack
                             (update-stack-machine
                              (+ (get-IC stack-machine) 1)
                              'INSTRUCTION-COUNTER
                              stack-machine))))))
                ;; POP_JUMP_IF_FALSE(target)
                ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'POP_JUMP_IF_FALSE)
                         (run-stack-machine
                          (if (equal? (top (get-stack stack-machine)) #f)
                              (pop-exec-stack
                               (update-stack-machine
                                (/ (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))) 2)
                                'INSTRUCTION-COUNTER
                                stack-machine))
                              (pop-exec-stack
                               (update-stack-machine
                                (+ (get-IC stack-machine) 1)
                                'INSTRUCTION-COUNTER
                                stack-machine)))))
                ;; POP_JUMP_IF_TRUE(target)
                ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'POP_JUMP_IF_TRUE)
                         (run-stack-machine
                          (if (equal? (top (get-stack stack-machine)) #t)
                              (pop-exec-stack
                               (update-stack-machine
                                (/ (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))) 2)
                                'INSTRUCTION-COUNTER
                                stack-machine))
                              (pop-exec-stack
                               (update-stack-machine
                                (+ (get-IC stack-machine) 1)
                                'INSTRUCTION-COUNTER
                                stack-machine)))))
                ;; JUMP_ABSOLUTE
                ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'JUMP_ABSOLUTE)
                         (run-stack-machine
                          (update-stack-machine
                           (/ (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))) 2)
                           'INSTRUCTION-COUNTER
                           stack-machine)))
                ;; FOR_ITER
                ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'FOR_ITER)
                         (run-stack-machine
                          (if (list? (top (get-stack stack-machine)))
                              (if (empty? (top (get-stack stack-machine)))
                                  (pop-exec-stack
                                   (update-stack-machine
                                    (+ (get-IC stack-machine) (/ (+ (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))) 2) 2))
                                    'INSTRUCTION-COUNTER
                                    stack-machine))
                                  (push-exec-stack
                                   (car (top (get-stack stack-machine)))
                                   (push-exec-stack
                                    (cdr (top (get-code stack-machine)))
                                    (pop-exec-stack stack-machine))))
                               (update-stack-machine
                                (+ (get-IC stack-machine) 1)
                                'INSTRUCTION-COUNTER
                                stack-machine))))
                (else
                 (update-stack-machine
                  (+ (get-IC stack-machine) 1)
                  'INSTRUCTION-COUNTER
                  stack-machine))
                )))