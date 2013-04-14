;;;; Provides keyword/value list operations.
;;
;; Copyright (c) 2006-2007 Arto Bendiken <http://bendiken.net/>
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;; IN THE SOFTWARE.

(module kvlists
  (keyword->symbol
   symbol->keyword
   alist?
   alist->kvlist
   alist->plist
   plist?
   plist->kvlist
   plist->alist
   kvlist?
   kvlist-key?
   kvlist-ref
   kvlist-set!
   kvlist-fold
   kvlist-map
   kvlist-for-each
   kvlist-assoc
   kvlist-cons*
   kvlist-delete
   kvlist-delete!
   kvlist->plist
   kvlist->alist)
  (import chicken
          data-structures
          scheme)
  (use srfi-1)

  ;; Keyword <-> symbol conversions (not provided by Chicken, at the moment)

  (define (symbol->keyword symbol)
    (string->keyword (symbol->string symbol)))

  (define (keyword->symbol keyword)
    (string->symbol (keyword->string keyword)))

  ;; Key-value list predicates

  (define (alist? x)
    (and (proper-list? x)
         (every pair? x)))

  (define (plist? x)
    (and (proper-list? x)
         (= (modulo (length x) 2) 0)))

  (define (kvlist? x)
    (and (proper-list? x)
         (= (modulo (length x) 2) 0)
         (every (lambda (kv) (keyword? (car kv)))
                (chop x 2))))

  ;; Key-value list accessors and operations (as provided by SRFI-1 for alists)
  ;; (Note that these procedures currently assume that keywords occur only as
  ;; keys, never as values. This should probably be corrected.)

  (define (kvlist-key? kvlist key)
    (not (not (memq key kvlist))))

  (define (kvlist-ref kvlist key #!optional default)
    (cond ((memq key kvlist) => cadr)
          (else default)))

  (define (kvlist-set! kvlist key value)
    (cond ((memq key kvlist) => (lambda (kv*) (set-car! (cdr kv*) value)))
          (else (append! kvlist (list (if (keyword? key) key
                                          (symbol->keyword key))
                                      value)))))

  (define (kvlist-fold cons nil kvlist)
    (fold (lambda (kv a) (cons (car kv) (cadr kv) a))
          nil
          (chop kvlist 2)))

  (define (kvlist-map proc kvlist)
    (map! (lambda (kv)
            (proc (car kv) (cadr kv)))
          (chop kvlist 2)))

  (define (kvlist-for-each proc kvlist)
    (for-each (lambda (kv)
                (proc (car kv) (cadr kv)))
      (chop kvlist 2)))

  (define (kvlist-assoc key kvlist)
    (cond ((memq key kvlist) => (lambda (kv*) (cons (car kv*) (cadr kv*))))
          (else #f)))

  (define (kvlist-cons* key value kvlist)
    (cons* key value kvlist))

  (define (kvlist-delete key kvlist)
    (concatenate! (remove! (lambda (kv) (eq? (car kv) key))
                           (chop kvlist 2))))

  (define kvlist-delete! kvlist-delete)

  ;; Key-value list <-> association list conversions

  (define (kvlist->alist kvlist)
    (map! (lambda (kv)
            (cons (keyword->symbol (car kv)) (cadr kv)))
          (chop kvlist 2)))

  (define (alist->kvlist alist)
    (append-map! (lambda (kv)
                   (list (symbol->keyword (car kv)) (cdr kv)))
                 alist))

  ;; Key-value list <-> property list conversions

  (define-inline (kv-mutator car-mutator)
    (lambda (kv)
      (set-car! kv (car-mutator (car kv))) kv))

  (define (plist->kvlist plist)
    (append-map! (kv-mutator symbol->keyword)
                 (chop plist 2)))

  (define (kvlist->plist kvlist)
    (append-map! (kv-mutator keyword->symbol)
                 (chop kvlist 2)))

  ;; Property list <-> association list conversions
  ;; (Simple & naive implementation as these are provided as a bonus only.)

  (define (plist->alist plist)
    (kvlist->alist (plist->kvlist plist)))

  (define (alist->plist alist)
    (kvlist->plist (alist->kvlist alist))))
