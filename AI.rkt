;; Part C
;; Karen Xu
;; University of Chicago


#lang typed/racket
(require typed/test-engine/racket-tests)
(require typed/2htdp/image)
(require typed/2htdp/universe)
(require "uchicago151.rkt")
(require "Interface.rkt")
(require "Structure.rkt")


(define-type Heuristic
  (Player Board -> Integer))


;; Heuristic functions

(: chain-exist : Board Player Loc Integer Integer (Loc -> Loc) -> Integer)
;; calculates length of a run
;; set run and n = 0, -1 means no run is possible
(define (chain-exist b player l n run func)
  (if (= n 5)
      run
      (cond
        [(equal? (board-ref b l) 'none)
         (chain-exist b player (func l) (add1 n) run func)]
        [(equal? (board-ref b l) (Some player))
         (chain-exist b player (func l)(add1 n) (add1 run) func)]
        [else -1])))
(check-expect
 (chain-exist welles 'white (Loc 0 1) 0 0
              (λ ([l : Loc]) (Loc (Loc-row l) (add1 (Loc-col l))))) 2)
(check-expect
 (chain-exist welles 'black (Loc 1 0) 0 0
              (λ ([l : Loc]) (Loc (Loc-row l) (add1 (Loc-col l))))) 1)

(: h-catalog : Player Board Integer -> (Listof Integer))
;; creates list of all the runs
;; pass 0 for i
(define (h-catalog p b i)
  (local
    {(: h-chain : Loc -> Loc)
     ;; changes location for horizontal chain
     (define (h-chain l)
       (match l
         [(Loc r c) (Loc r (add1 c))]))}
    (if (= i 12)
        '()
        (cons (chain-exist b p (Loc (quotient i 2) (modulo i 2)) 0 0 h-chain)
              (h-catalog p b (add1 i))))))
(check-expect (h-catalog 'white welles 0) '(1 2 -1 0 1 1 1 1 -1 -1 -1 -1))
(check-expect (h-catalog 'black welles 0) '(-1 -1 1 0 -1 -1 -1 -1 -1 -1 1 2))

(: v-catalog : Player Board Integer -> (Listof Integer))
;; creates list of all the runs
(define (v-catalog p b i)
  (local
    {(: v-chain : Loc -> Loc)
     ;; changes location for veritcal chain
     (define (v-chain l)
       (match l
         [(Loc r c) (Loc (add1 r) c)]))}
    (if (= i 12)
        '()
        (cons (chain-exist b p (Loc (modulo i 2) (quotient i 2)) 0 0 v-chain)
              (v-catalog p b (add1 i))))))
(check-expect (v-catalog 'white welles 0) '(-1 -1 1 -1 1 0 1 1 -1 -1 1 -1))
(check-expect (v-catalog 'black welles 0) '(1 1 -1 -1 -1 0 -1 -1 -1 -1 -1 1))

(: d1-catalog : Player Board Integer -> (Listof Integer))
;; creates list of all the runs
(define (d1-catalog p b i)
  (local
    {(: d1-chain : Loc -> Loc)
     ;; changes location for l to r diagonal chain
     (define (d1-chain l)
       (match l
         [(Loc r c) (Loc (add1 r) (add1 c))]))}
    (if (= i 4)
        '()
        (cons (chain-exist b p (Loc (quotient i 2) (modulo i 2)) 0 0 d1-chain)
              (d1-catalog p b (add1 i))))))
(check-expect (d1-catalog 'white welles 0) '(-1 2 -1 -1))
(check-expect (d1-catalog 'black welles 0) '(1 -1 1 2))

(: d2-catalog : Player Board Integer -> (Listof Integer))
;; creates list of all the runs
(define (d2-catalog p b i)
  (local
    {(: d2-chain : Loc -> Loc)
     ;; changes location for r to l diagonal chain
     (define (d2-chain l)
       (match l
         [(Loc r c) (Loc (add1 r) (sub1 c))]))}
    (if (= i 4)
        '()
        (cons (chain-exist b p (Loc (quotient i 2) (+ 4 (modulo i 2)))
                           0 0 d2-chain)
              (d2-catalog p b (add1 i))))))
(check-expect (d2-catalog 'white welles 0) '(0 3 2 -1))
(check-expect (d2-catalog 'black welles 0) '(0 -1 -1 1))

(: long-run : Heuristic)
; compute the length of the longest run on the board
(define (long-run p b)
  (local
    {(: maxrun : (Listof Integer) Integer -> Integer)
     ;; return max values of a list of integers
     (define (maxrun list acc)
       (match list
         ['() acc]
         [(cons f r) (if (>= f acc ) (maxrun r f) (maxrun r acc))]))} 
    (maxrun (append (h-catalog p b 0) (v-catalog p b 0)
                    (d1-catalog p b 0) (d2-catalog p b 0)) -1)))
(check-expect (long-run 'white welles) 3)
(check-expect (long-run 'black welles) 2)


(: sum-squares : Heuristic)
;; compute the sum of the squares of all runs on the board
(define (sum-squares p b)
  (foldr + 0 (map sqr (append (h-catalog p b 0) (v-catalog p b 0)
                              (d1-catalog p b 0) (d2-catalog p b 0)))))
(check-expect (sum-squares 'white welles) 46)
(check-expect (sum-squares 'black welles) 35)


(: long-run-difference : Heuristic)
;; compute the long run for current player minus the long run of opponent
(define (long-run-difference p b)
  (if (equal? p 'white)
      (- (long-run 'white b) (long-run 'black b))
      (- (long-run 'black b) (long-run 'white b))))
(check-expect (long-run-difference 'white welles) 1)
(check-expect (long-run-difference 'black welles) -1)


(: sum-squares-difference : Heuristic)
;; compute sum-squares of current player minus sum-squares of opponent
(define (sum-squares-difference p b)
  (if (equal? p 'white)
      (- (sum-squares 'white b) (sum-squares 'black b))
      (- (sum-squares 'black b) (sum-squares 'white b))))
(check-expect (sum-squares-difference 'white welles) 11)
(check-expect (sum-squares-difference 'black welles) -11)




;; Putting the Pieces Together

(: direct-win : Player Board -> (Optional (U Loc Move)))
;; report a move that results in an immediate victory if there is one
(define (direct-win p b)
  (local
    {(: helper : Integer -> (Optional (U Loc Move)))
     ;; goes through list of all-possible moves and checks if it's a win
     (define (helper n)
       (define list-of-moves (all-possible p b))
       (define move (list-ref list-of-moves n))
       (if (= n (sub1 (length list-of-moves)))
           'none
           (cond
             [(game-over?
               (place-marble (Game b p 'place)
                             p
                             (Move-loc move))) (Some (Move-loc move))]
             [(game-over? (twist-quadrant
                           (place-marble (Game b p 'place)
                                         p
                                         (Move-loc move))
                           (Move-q move)
                           (Move-dir move))) (Some move)]
             [else (helper (add1 n))])))}
    (helper 0)))
(check-expect (direct-win 'black stanton)
              (Some (Move (Loc 1 1) 'NW 'counterclockwise)))
(check-expect (direct-win 'white stanton)
              (Some (Move (Loc 0 1) 'NE 'counterclockwise)))
(check-expect (direct-win 'white welles) 'none)


(: emptyspaces : Board Integer -> (Listof Loc))
;; helper for all-possible
;; returns all empty spaces, pass 0 for n
(define (emptyspaces b n)
  (cond
    [(= n 36) '()]
    [else (cond
            [(equal? (board-ref b (Loc (quotient n 6) (modulo n 6))) 'none)
             (cons (Loc (quotient n 6) (modulo n 6)) (emptyspaces b (add1 n)))]
            [else (emptyspaces b (add1 n))])]))
(check-expect (length (emptyspaces welles 0)) 27)
(check-expect (length (emptyspaces stanton 0)) 12)
(check-expect (emptyspaces jackson 0) '())


(: all-possible : Player Board -> (Listof Move))
; return the list of all possible moves from the current board
(define (all-possible p b)
  (local
    {(: all : Board Integer (Listof Loc) -> (Listof Move))
     ;; set n = 0
     (define (all b n l)
       (if (= n (length l)) '()
           (cons
            (Move (list-ref l n) 'NW 'clockwise)
            (cons
             (Move (list-ref l n) 'NW 'counterclockwise)
             (cons
              (Move (list-ref l n) 'NE 'clockwise)
              (cons
               (Move (list-ref l n) 'NE 'counterclockwise)
               (cons
                (Move (list-ref l n) 'SW 'clockwise)
                (cons
                 (Move (list-ref l n) 'SW 'counterclockwise)
                 (cons
                  (Move (list-ref l n) 'SE 'clockwise)
                  (cons
                   (Move (list-ref l n) 'SE 'counterclockwise)
                   (all b (add1 n) l)))))))))))}
    (all b 0 (emptyspaces b 0))))
(check-expect (length (all-possible 'white stanton)) 96)
(check-expect (all-possible 'white jackson) '())


(: direct-win-move : Player Board -> (Optional Move))
;; helper for make-mind, same as direct-win but returns only moves
(define (direct-win-move p b)
  (local
    {(: helper : Integer -> (Optional Move))
     ;; goes through list of all-possible moves and checks if it's a win
     (define (helper n)
       (define list-of-moves (all-possible p b))
       (define move (list-ref list-of-moves n))
       (if (= n (sub1 (length list-of-moves)))
           'none
           (cond
             [(game-over? (place-marble (Game b p 'place)
                                        p
                                        (Move-loc move)))
              (Some (Move (Move-loc move) 'NW 'clockwise))]
             [(game-over? (twist-quadrant
                           (place-marble (Game b p 'place)
                                         p
                                         (Move-loc move))
                           (Move-q move)
                           (Move-dir move))) (Some move)]
             [else (helper (add1 n))])))}
    (helper 0)))


(: make-mind : Player Heuristic -> (Game -> (Optional Move)))
;; return direct-win move or best move according to heuristic
(define (make-mind p lr)
  (local
    {(: best : (Listof Integer) Integer Integer Integer -> Integer)
     ;; run through list of longest runs of each board of each possible move
     ;; returns index x of longest run , set n and x = 0
     (define (best list acc n x)
       (match list
         ['() x]
         [(cons f r)
          (if (>= f acc) (best r f (add1 n) n) (best r acc (add1 n) x))]))
     (: mind : (Game -> (Optional Move)))
     ;; returns index of move which gives longest run
     (define (mind g)
       (match g
         [(Game b player a)
          (local
            {(: m->b : Move -> Board)
             ;; takes a possible moves and returns board after move is done
             (define (m->b m)
               (match m
                 [(Move l q d)
                  (Game-board (twist-quadrant
                               (place-marble (Game b p 'place) p l)
                               q
                               d))]))
             (define moves (all-possible p b))
             (define longrun-list ;; moves->boards->integers
               (map (λ ([b : Board]) (lr p b)) (map m->b moves)))}
            (cond
              [(equal? (direct-win-move p b) 'none)
               (Some (list-ref moves (best longrun-list -1 0 0)))]
              [else (direct-win-move p b)]))]
         [_ (error "No game")]))}
    mind))
(check-expect ((make-mind 'white long-run) (Game welles 'white 'place))
              (Some (Move (Loc 5 2) 'SW 'counterclockwise)))
(check-expect ((make-mind 'black long-run) (Game welles 'black 'place))
              (Some (Move (Loc 5 4) 'NE 'counterclockwise)))



(test)