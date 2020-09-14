#lang racket

(struct space
  (underlying-set
   topology)
  #:guard
  (λ (underlying-set
      topology this)
    (cond
      [(not (set? underlying-set))
       (error this "~a is not a set."
              underlying-set)]
      [(not (set? topology))
       (error this "~a is not a set."
              topology)]
      [(not
        (and
         (set-member? topology
                      underlying-set)
         (set-member? topology
                      (set))
         (for*/and
             ([i (in-set topology)]
              [j (in-set topology)])
           (and (set-member?
                 topology
                 (set-union i j))
                (set-member?
                 topology
                 (set-intersect i j))))))
       (error this
              "~a is not a valid topology."
              topology)])
    (values underlying-set
            topology)))

(define (power-set st)
  (list->set
   (map list->set
        (combinations (set->list st)))))

(define (map-set f st)
  (for/set ([i (in-set st)])
    (f i)))

(define (set-filter p st)
  (for/set ([i (in-set st)]
            #:when (p i))
    i))

(define (image f st1 st2 sbst)
  (if (not (subset? sbst st1))
      (error "~a is not a subset of ~a"
             sbst st1)
      (set-intersect
       (map-set f sbst) st2)))

(define (preimage f st1 st2 sbst)
  (if (not (subset? sbst st2))
      (error "~a is not a subset of ~a"
             sbst st2)
      (for/set ([e (in-set st1)]
                #:when
                (set-member? sbst (f e)))
        e)))

(define (subspace spc sbst)
  (let
      ([u (space-underlying-set
           spc)]
       [t (space-topology
           spc)])
    (if (not (subset? sbst u))
        (error
         sbst "Not a subset of ~a" spc)
        (space sbst
               (map-set
                (λ (x)
                  (set-intersect sbst x))
                t)))))

(define
  (set->discrete-space st)
  (space st (power-set st)))

(define
  (set->indiscrete-space st)
  (space st (set (set) st)))

(define
  (discrete-space? spc)
  (let
      ([u (space-underlying-set
           spc)]
       [t (space-topology
           spc)])
    (set=? t (power-set u))))

(define
  (indiscrete-space? spc)
  (let
      ([u (space-underlying-set
           spc)]
       [t (space-topology
           spc)])
    (set=? t (set (set) u))))

(define (open-set? spc st)
  (set-member? (space-topology spc) st))

(define (closed-set? spc st)
  (open-set?
   spc
   (set-subtract (space-underlying-set spc)
                 st)))

(define (clopen-set? spc st)
  (and (open-set? spc st)
       (closed-set? spc st)))

(define (closed-sets spc)
  (let
      ([u (space-underlying-set
           spc)]
       [t (space-topology
           spc)])
    (map-set
     (λ(x)(set-subtract u x))
     t)))

(define
  (closure sbst spc)
  (let
      ([u (space-underlying-set
           spc)]
       [t (space-topology
           spc)])
    (if (not (subset? sbst u))
        (error
         sbst "Not a subset of ~a" spc)
        (for/fold ([acc u])
                  ([i (in-set
                       (closed-sets spc))]
                   #:when
                   (and
                    (subset? sbst i)
                    (closed-set? spc i)))
          (set-intersect acc i)))))

(define
  (interior sbst spc)
  (let
      ([u (space-underlying-set
           spc)]
       [t (space-topology
           spc)])
    (if (not (subset? sbst u))
        (error
         sbst "Not a subset of ~a" spc)
        (for/fold ([acc u])
                  ([i (in-set t)]
                   #:when
                   (and
                    (subset? i sbst)
                    (open-set? spc i)))
          (set-intersect acc i)))))
(define
  (boundary sbst spc)
  (let*
      ([u (space-underlying-set
           spc)]
       [bndry (set-subtract
               (closure sbst spc)
               (interior sbst spc))])
    (if (not (subset? sbst u))
        (error
         sbst "Not a subset of ~a" spc)
        (when (open-set? spc bndry)
          bndry))))

(define
  (exterior sbst spc)
  (let*
      ([u (space-underlying-set
           spc)]
       [ext (set-subtract
             u (closure sbst spc))])
    (if (not (subset? sbst u))
        (error
         sbst "Not a subset of ~a" spc)
        (when (open-set? spc ext)
          ext))))

(define
  (continuous-function? f spc1 spc2)
  (and (space? spc1)
       (space? spc2)
       (subset?
        (map-set
         f (space-underlying-set spc1))
        (space-underlying-set spc2)
        )
       (for/and ([open
                  (in-set
                   (space-topology spc2))])
         (open-set?
          spc1
          (preimage
           f
           (space-underlying-set spc1)
           (space-underlying-set spc2)
           open))
         )))

(define (open-map? f spc1 spc2)
  (let ([t1 (space-topology spc1)])
    (and (continuous-function? f spc1 spc2)
         (for/and ([i (in-set t1)])
           (and (open-set? spc1 i)
                (open-set? spc2 (f i)))))))

(define (closed-map? f spc1 spc2)
  (let ([t1 (space-topology spc1)])
    (and (continuous-function? f spc1 spc2)
         (for/and ([i (in-set t1)])
           (and (closed-set? spc1 i)
                (closed-set? spc2 (f i)))))))

(define empty-space
  (set->discrete-space (set)))

(define point-space
  (set->discrete-space (set 0)))

(define Sierpiński-space
  (space (set 0 1)
         (set-remove
          (power-set (set 0 1))
          (set 0))))