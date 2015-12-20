;; CMSC 15100, Autumn 2015, University of Chicago
;; Project C
;; Karen Xu

#lang typed/racket
(require typed/test-engine/racket-tests)
(require typed/2htdp/image)
(require typed/2htdp/universe)
(require "uchicago151.rkt")

;; === DATA DEFINITIONS

(define-type (Optional a)
  (U 'none (Some a)))

(define-struct (Some a)
  ([x : a]))

(define-type Player 
  (U 'black 'white))

(define-struct Loc
  ([row : Integer]   ;; an integer on the interval [0,5]
   [col : Integer])) ;; an integer on the interval [0,5]

(define-type Quadrant
  (U 'NW 'NE 'SW 'SE))

(define-type Direction
  (U 'clockwise 'counterclockwise))

(define-struct Board
  ([NW : (Listof (Optional Player))] ;; these are all lists of length 9
   [NE : (Listof (Optional Player))]
   [SW : (Listof (Optional Player))]
   [SE : (Listof (Optional Player))]))

(define-struct Game
  ([board : Board]
   [next-player : Player]
   [next-action : (U 'place 'twist)]))

(define-type Outcome
  (U Player 'tie))

(define-struct Move
  ([loc : Loc]
   [q   : Quadrant]
   [dir : Direction]))

(define-type Human
  (U String Symbol))

(define-struct Bot
  ([name : (U String Symbol)]
   [mind : (Game -> (Optional Move))]))

(define-struct World
  ([game : Game]
   [row : Integer]
   [col : Integer]
   [player1 : (U Human Bot)] ;; white player
   [player2 : (U Human Bot)] ;; black player
   [strategy : (Game -> (Optional Move))]))



;; === SAMPLE BOARDS

(define grant
  (Board (list (Some 'black) 'none 'none 'none 'none 'none 'none 'none 'none)
         (list 'none 'none 'none 'none 'none 'none 'none 'none 'none)
         (list 'none 'none 'none 'none 'none 'none 'none 'none 'none)
         (list 'none (Some 'white) 'none 'none 'none 'none 'none 'none 'none)))
(define grant-game (Game grant 'white 'place))

;; horizontal 5 for black
(define sherman
  (Board (list (Some 'black) (Some 'black) (Some 'black) 'none 'none 'none
               'none 'none 'none)
         (list (Some 'black) (Some 'black) 'none 'none 'none 'none
               'none 'none 'none)
         (list 'none 'none 'none 'none 'none 'none 'none 'none 'none)
         (list 'none (Some 'white) 'none 'none 'none 'none 'none 'none 'none)))       
;; vertical 5 for white
(define scott
  (Board (list (Some 'black) 'none (Some 'black) 'none 'none 'none
               'none 'none 'none)
         (list (Some 'black) 'none 'none 'none (Some 'white) 'none
               'none (Some 'white) 'none)
         (list 'none 'none 'none 'none (Some 'white) 'none
               'none (Some 'white) 'none)
         (list 'none (Some 'white) 'none 'none (Some 'white) 'none
               'none (Some 'white) 'none)))
;; diagonal1 5 for black
(define lee
  (Board (list 'none 'none (Some 'black) (Some 'black) 'none 'none
               'none (Some 'black) 'none)
         (list (Some 'black) 'none 'none 'none (Some 'white) 'none
               'none (Some 'white) 'none)
         (list 'none 'none (Some 'black) 'none 'none 'none
               'none (Some 'white) 'none)
         (list 'none (Some 'white) 'none (Some 'black) 'none 'none
               'none (Some 'black) 'none)))
;; diagonal2 5 for white
(define harrison
  (Board (list 'none 'none (Some 'black) 'none 'none 'none 'none 'none 'none)
         (list 'none 'none (Some 'white) 'none (Some 'white) 'none
               (Some 'white) 'none 'none)
         (list 'none 'none (Some 'white) 'none (Some 'white) 'none
               'none (Some 'black) 'none)
         (list 'none (Some 'white) 'none 'none (Some 'black) 'none
               'none 'none (Some 'black))))

;; scattered board
(define welles
  (Board (list 'none 'none (Some 'white) (Some 'black) 'none 'none
               'none 'none 'none)
         (list 'none 'none (Some 'white) 'none 'none 'none
               (Some 'white) 'none 'none)
         (list 'none 'none 'none 'none (Some 'white) 'none
               'none (Some 'black) 'none)
         (list 'none (Some 'white) 'none 'none (Some 'black) 'none
               'none 'none (Some 'black))))
;; fuller scattered board
(define stanton
  (Board (list (Some 'white) 'none (Some 'white) (Some 'black) 'none 'none
               (Some 'black) (Some 'black) (Some 'white))
         (list 'none 'none (Some 'white) (Some 'black) 'none (Some 'white)
               (Some 'white) (Some 'black) (Some 'white))
         (list 'none 'none (Some 'white) (Some 'black) (Some 'white) 'none
               (Some 'black) (Some 'black) (Some 'white))
         (list (Some 'black) (Some 'white) 'none 'none (Some 'black) 'none
               (Some 'black) (Some 'white) (Some 'black))))
(define stanton2
  (Board (list (Some 'white) 'none (Some 'white) (Some 'black) 'none 'none
               (Some 'black) (Some 'black) (Some 'white))
         (list 'none 'none (Some 'white) (Some 'black) 'none (Some 'white)
               (Some 'white) (Some 'black) (Some 'white))
         (list 'none 'none (Some 'white) 'none (Some 'white) 'none
               (Some 'black) (Some 'black) (Some 'white))
         (list (Some 'black) (Some 'white) 'none 'none (Some 'black) 'none
               (Some 'black) (Some 'white) (Some 'black))))
;; full board
(define jackson
  (Board
   (list (Some 'white) (Some 'black) (Some 'black)
         (Some 'black) (Some 'black) (Some 'black)
         (Some 'black) (Some 'black) (Some 'black))
   (list (Some 'white) (Some 'white) (Some 'black)
         (Some 'white) (Some 'white) (Some 'white)
         (Some 'white) (Some 'white) (Some 'white))
   (list (Some 'white) (Some 'white) (Some 'white)
         (Some 'white) (Some 'white) (Some 'white)
         (Some 'black) (Some 'white) (Some 'white))
   (list (Some 'black) (Some 'black) (Some 'black)
         (Some 'black) (Some 'black) (Some 'black)
         (Some 'black) (Some 'black) (Some 'white))))









;; === PROJECT A



;; GAME LOGIC -------------------------------------------------------------

(: new-game : Game)
;; game starts with an empty board, white moves first
(define empty
  (list 'none 'none 'none 'none 'none 'none 'none 'none 'none))
(define new-game
  (Game (Board empty empty empty empty) 'white 'place))


; board-ref ---------------------------------

(: index : Loc -> Integer)
;; computes where to list-ref
(define (index loc)
  (match loc
    [(Loc r c) (+ (* (modulo r 3) 3) (modulo c 3))] ))
(check-expect (index (Loc 0 0)) 0)
(check-expect (index (Loc 3 4)) 1)

(: quad-ref : Loc -> (Board -> (Listof (Optional Player))))
;; return function that extracts list from Board
(define (quad-ref loc)
  (match loc
    [(Loc r c) (cond
                 [(and (< r 3) (< c 3)) Board-NW]
                 [(and (< r 3) (>= c 3)) Board-NE]
                 [(and (>= r 3) (< c 3)) Board-SW]
                 [else Board-SE] )] ))
(check-expect ((quad-ref (Loc 0 0)) grant)
              (list (Some 'black) 'none 'none 'none 'none 'none
                    'none 'none 'none))
(check-expect ((quad-ref (Loc 3 4)) grant)
              (list 'none (Some 'white) 'none 'none 'none 'none
                    'none 'none 'none))

(: board-ref : Board Loc -> (Optional Player))
;; return the marble at the given square, or 'none
(define (board-ref b l)
  (list-ref ((quad-ref l) b) (index l) ))
(check-expect (board-ref grant (Loc 0 0)) (Some 'black))
(check-expect (board-ref grant (Loc 3 4)) (Some 'white))

; place-marble ------------------------------

(: relist :
   (Listof (Optional Player)) Integer Player -> (Listof (Optional Player)))
;; runs through list and changes element of list at given position
(define (relist l n p)
  (match l
    ['() '()]
    [(cons f r) (if (= n 0)
                    (if (equal? f 'none)
                        (cons (Some p) r)
                        (error "invalid move, spot already taken"))
                    (cons f (relist r (sub1 n) p)))] ))
(check-expect (relist (list 'none (Some 'white) 'none
                            'none (Some 'black) 'none
                            'none 'none (Some 'black)) 7 'black)
              (list 'none (Some 'white) 'none
                    'none (Some 'black) 'none
                    'none (Some 'black) (Some 'black)))       

(: place-marble : Game Player Loc -> Game)
;; place the marble on the game board and return new game state
;; if move is illegal raise error
(define (place-marble g p l)
  (match g
    [(Game _ _ 'twist) (error "invalid move, twist is next")]
    [(Game b q 'place)
     (if (symbol=? p q)
         (Game
          (match l
            [(Loc r c)
             (match b
               [(Board nw ne sw se)
                (cond
                  [(and (< r 3) (< c 3))
                   (Board (relist ((quad-ref l) b) (index l) p) ne sw se)]
                  [(and (< r 3) (>= c 3))
                   (Board nw (relist ((quad-ref l) b) (index l) p) sw se)]
                  [(and (>= r 3) (< c 3))
                   (Board nw ne (relist ((quad-ref l) b) (index l) p) se)]
                  [else
                   (Board nw ne sw (relist ((quad-ref l) b) (index l) p) )])])])
          p
          'twist)
         (error "wrong player"))]
    ))
(check-expect (place-marble (Game grant 'white 'place) 'white (Loc 1 1))
              (Game (Board (list (Some 'black) 'none 'none
                                 'none (Some 'white) 'none 'none 'none 'none)
                           (list 'none 'none 'none 'none 'none 'none
                                 'none 'none 'none)
                           (list 'none 'none 'none 'none 'none 'none
                                 'none 'none 'none)
                           (list 'none (Some 'white) 'none 'none 'none 'none
                                 'none 'none 'none))
                    'white 'twist))

; twist-quadrant ----------------------------

(: counter-twist : (Listof (Optional Player)) -> (Listof (Optional Player)))
;; creates new list when twisted counter-clockwise
(define (counter-twist l)
  (match l
    [(list a b c
           d e f
           g h i)
     (list c f i
           b e h
           a d g)] ))
(check-expect (counter-twist (list 'none (Some 'white) 'none
                                   'none (Some 'black) 'none
                                   'none 'none (Some 'black)))
              (list 'none 'none (Some 'black)
                    (Some 'white) (Some 'black) 'none
                    'none 'none 'none))

(: clock-twist : (Listof (Optional Player)) -> (Listof (Optional Player)))
;; creates new list when twisted counter-clockwise
(define (clock-twist l)
  (match l
    [(list a b c
           d e f
           g h i)
     (list g d a
           h e b
           i f c)] ))
(check-expect (clock-twist (list 'none (Some 'white) 'none
                                 'none (Some 'black) 'none
                                 'none 'none (Some 'black)))
              (list 'none 'none 'none
                    'none (Some 'black) (Some 'white)
                    (Some 'black) 'none 'none))

(: twist : Board Quadrant Direction -> Board)
;; creates new list depending on Quadrant and Direction
(define (twist b q d)
  (match b
    [(Board nw ne sw se)
     (match d
       ['counterclockwise (match q
                            ['NW (Board (counter-twist nw) ne sw se)]
                            ['NE (Board nw (counter-twist ne) sw se)]
                            ['SW (Board nw ne (counter-twist sw) se)]
                            ['SE (Board nw ne sw (counter-twist se))] )]
       ['clockwise (match q
                     ['NW (Board (clock-twist nw) ne sw se)]
                     ['NE (Board nw (clock-twist ne) sw se)]
                     ['SW (Board nw ne (clock-twist sw) se)]
                     ['SE (Board nw ne sw (clock-twist se))] )] )] ))
(check-expect (twist harrison 'SE 'clockwise)
              (Board (list 'none 'none (Some 'black) 'none 'none 'none
                           'none 'none 'none)
                     (list 'none 'none (Some 'white) 'none (Some 'white) 'none
                           (Some 'white) 'none 'none)
                     (list 'none 'none (Some 'white) 'none (Some 'white) 'none
                           'none (Some 'black) 'none)
                     (list 'none 'none 'none 'none (Some 'black) (Some 'white)
                           (Some 'black) 'none 'none)))


(: twist-quadrant : Game Quadrant Direction -> Game)
;; twist a quadrant clockwise/counterclockwise and return new game state
(define (twist-quadrant g q d)
  (match g
    [(Game _ _ 'place) (error "invalid move, place is next")]
    [(Game b p 'twist) (Game (twist b q d)
                             (if (symbol=? p 'white) 'black 'white)
                             'place)] ))
(check-expect
 (twist-quadrant (Game harrison 'white 'twist) 'SE 'clockwise)
 (Game (Board (list 'none 'none (Some 'black) 'none 'none 'none
                    'none 'none 'none)
              (list 'none 'none (Some 'white) 'none (Some 'white) 'none
                    (Some 'white) 'none 'none)
              (list 'none 'none (Some 'white) 'none (Some 'white) 'none
                    'none (Some 'black) 'none)
              (list 'none 'none 'none 'none (Some 'black) (Some 'white)
                    (Some 'black) 'none 'none)) 'black 'place))

; game-over? ---------------------------------

(: h-five : Board Loc Integer -> (Optional Player))
;; checks if there's HORIZONTAL five in a row from given starting point
;; pass 3 in for n
(define (h-five b l n)
  (match l
    [(Loc r c) (cond
                 [(equal? (board-ref b l) 'none) 'none]
                 [(equal? (board-ref b l) (board-ref b (Loc r (add1 c)) )) 
                  (if (= n 0)
                      (board-ref b l)
                      (h-five b (Loc r (add1 c)) (sub1 n)) )]
                 [else 'none] )]
    [_ (error "error in loc")] ))
(check-expect (h-five sherman (Loc 0 0) 3) (Some 'black))

(: all-h-five : Board Integer -> (Optional Player))
;; checks if there are any horizontal five in a rows
;; pass 11 in for k
(define (all-h-five b k)
  (match k
    [-1 'none]
    [_ (match (h-five b (Loc (modulo k 6) (quotient k 6)) 3)
         [(Some a) (Some a)] 
         ['none (all-h-five b (sub1 k))]
         [_ (error "error")] )]))
(check-expect (all-h-five sherman 11) (Some 'black))

(: v-five : Board Loc Integer -> (Optional Player))
;; checks if there's VERTICAL five in a row from given starting point
(define (v-five b l n)
  (match l
    [(Loc r c) (cond
                 [(equal? (board-ref b l) 'none) 'none]
                 [(equal? (board-ref b l) (board-ref b (Loc (add1 r) c))) 
                  (if (= n 0)
                      (board-ref b l)
                      (v-five b (Loc (add1 r) c) (sub1 n)) )]
                 [else 'none] )]
    [_ (error "error in loc")] ))
(check-expect (v-five scott (Loc 1 4) 3) (Some 'white))

(: all-v-five : Board Integer -> (Optional Player))
;; checks if there are any vertical five in a rows
(define (all-v-five b k)
  (match k
    [-1 'none]
    [_ (match (v-five b (Loc (quotient k 6) (modulo k 6)) 3)
         [(Some a) (Some a)] 
         [_ (all-v-five b (sub1 k))])] ))
(check-expect (all-v-five scott 11) (Some 'white))

(: d1-five : Board Loc Integer -> (Optional Player))
;; checks if there's DIAGONAL five in a row from given starting point 
(define (d1-five b l n)
  (match l
    [(Loc r c) (cond
                 [(equal? (board-ref b l) 'none) 'none]
                 [(equal? (board-ref b l) (board-ref b (Loc (add1 r) (add1 c))))
                  (if (= n 0)
                      (board-ref b l)
                      (d1-five b (Loc (add1 r) (add1 c)) (sub1 n)) )]
                 [else 'none] )]
    [_ (error "error in loc")] ))
(check-expect (d1-five lee (Loc 1 0) 3) (Some 'black))


(: all-d1-five : Board Integer -> (Optional Player))
;; checks if there are any diagonal five in a rows for (0,0) (1,1) (1,0) (0,1)
(define (all-d1-five b k)
  (match k
    [-1 'none]
    [_ (match (d1-five b (Loc (quotient k 2) (modulo k 2)) 3)
         [(Some a) (Some a)] 
         [_ (all-d1-five b (sub1 k))])] ))
(check-expect (all-d1-five lee 3) (Some 'black))

(: d2-five : Board Loc Integer -> (Optional Player))
;; checks if there's DIAGONAL five in a row from given starting point
(define (d2-five b l n)
  (match l
    [(Loc r c) (cond
                 [(equal? (board-ref b l) 'none) 'none]
                 [(equal? (board-ref b l) (board-ref b (Loc (add1 r) (sub1 c)))) 
                  (if (= n 0)
                      (board-ref b l)
                      (d2-five b (Loc (add1 r) (sub1 c)) (sub1 n)) )]
                 [else 'none] )]
    [_ (error "error in loc")] ))
(check-expect (d2-five harrison (Loc 0 5) 3) (Some 'white))


(: all-d2-five : Board Integer -> (Optional Player))
;; checks if there are any diagonal five in a rows for (0,0) (1,1) (1,0) (0,1)
(define (all-d2-five b k)
  (match k
    [-1 'none]
    [_ (match (d2-five b (Loc (quotient k 2) (+ (modulo k 2) 4)) 3)
         [(Some a) (Some a)] 
         [_ (all-d2-five b (sub1 k))])] ))
(check-expect (all-d2-five harrison 3) (Some 'white))


(: check-full? : (Listof (Optional Player)) Integer -> Boolean)
;; checks if one list is full 
;; pass 8 for n
(define (check-full? l  n)
  (if (= n 0)
      #t
      (match (list-ref l n)
        ['none #f]
        [(Some a) (check-full? l (sub1 n))] )))
(check-expect (check-full?  (list (Some 'black) (Some 'black) (Some 'black)
                                  (Some 'black) (Some 'black) (Some 'black)
                                  (Some 'black) (Some 'black) (Some 'white)) 8)
              #t)
(check-expect (check-full? empty 8) #f)


(: board-full? : Board -> Boolean)
;; checks if board is full
(define (board-full? b)
  (match b
    [(Board nw ne sw se) (and (check-full? nw 8) (check-full? ne 8)
                              (check-full? sw 8) (check-full? se 8))]))
(check-expect (board-full? lee) #f)
(check-expect (board-full? jackson) #t)

(: game-over? : Game -> Boolean)
;; return true if player has five in a row or board is full
(define (game-over? g)
  (match g
    [(Game b p a)
     (cond
       [(match (all-h-five b 11)
          [(Some a) #t]
          ['none #f]) #t]
       [(match (all-v-five b 11)
          [(Some a) #t]
          ['none #f]) #t]
       [(match (all-d1-five b 3)
          [(Some a) #t]
          ['none #f]) #t]
       [(match (all-d2-five b 3)
          [(Some a) #t]
          ['none #f]) #t]
       [(board-full? b) #t]
       [else #f])] ))
(check-expect (game-over? (Game jackson 'black 'twist)) #t)
(check-expect (game-over? (Game scott 'black 'place)) #t)
(check-expect (game-over? (Game grant 'white 'twist)) #f)

; outcome -----------------------------------

(: outcome : Game -> Outcome)
;; shows outcome of game, either tie, white, or black
(define (outcome g)
  (match g
    [(Game b p c)
     (if (game-over? g)
         (cond
           [(match (all-h-five b 11)
              [(Some a) #t]
              ['none #f]) (if (equal? (all-h-five b 11) (Some 'white))
                              'white
                              'black)]
           [(match (all-v-five b 11)
              [(Some a) #t]
              ['none #f]) (if (equal? (all-v-five b 11) (Some 'white))
                              'white
                              'black)]
           [(match (all-d1-five b 3)
              [(Some a) #t]
              ['none #f]) (if (equal? (all-d1-five b 3) (Some 'white))
                              'white
                              'black)]
           [(match (all-d2-five b 3)
              [(Some a) #t]
              ['none #f]) (if (equal? (all-d2-five b 3) (Some 'white))
                              'white
                              'black)]
           [(board-full? b) 'tie]
           [else (error "error")])
         (error "game not over"))] ))
(check-expect (outcome (Game jackson 'black 'twist)) 'tie)
(check-expect (outcome (Game scott 'black 'place)) 'white)
(check-expect (outcome (Game lee 'white 'twist)) 'black)



;; VISUALIZATION ----------------------------------------------------------

(: block : (Optional Player) Real -> Image)
;; builds one marble
(define (block o n)
  (overlay (circle (max (/ n 3) 0) "solid" (match o 
                                             [(Some 'white) 'white]
                                             [(Some 'black) 'black]
                                             [_ 'darkred]))
           (square (max n 0) "solid" (color 185 4 4 255))))

(: quad : (Listof (Optional Player)) Real -> Image)
;; builds one quadrant
(define (quad l n)
  (above
   (beside (block (list-ref l 0) (/ n 3))
           (block (list-ref l 1) (/ n 3))
           (block (list-ref l 2) (/ n 3)))
   (beside (block (list-ref l 3) (/ n 3))
           (block (list-ref l 4) (/ n 3))
           (block (list-ref l 5) (/ n 3))) 
   (beside (block (list-ref l 6) (/ n 3))
           (block (list-ref l 7) (/ n 3))
           (block (list-ref l 8) (/ n 3))) ))

(: four-quad : Board Integer -> Image)
;; builds four quads
(define (four-quad b n)
  (match b
    [(Board nw ne sw se)
     (if (>= n 0)
         (above (beside (quad nw (* n 40/100))
                        (rectangle (* n 5/100) (* n 40/100) "solid" 'white)
                        (quad ne (* n 40/100)))
                (rectangle (* n 85/100) (* n 5/100) "solid" 'white)
                (beside (quad sw (* n 40/100))
                        (rectangle (* n 5/100) (* n 40/100) "solid" 'white)
                        (quad se (* n 40/100))))
         (error "n is negative"))] ))

(: row : Integer -> Image)
;; creates label for row
(define (row n)
  (if (positive? n)
      (overlay
       (scale (/ n 100)
              (above (text "0" 14 'black)
                     (text "1" 14 'black)
                     (text "2" 14 'black)
                     (text "3" 14 'black)
                     (text "4" 14 'black)
                     (text "5" 14 'black)))
       (rectangle (max (* n 15/100) 0) (max (* n 85/100) 0)
                  "solid" 'white))
      (error "n must be positive") ))

(: col : Integer -> Image)
;; creates label for column
(define (col n)
  (if (positive? n)
      (overlay
       (scale-to-width
        (beside (square 16 "solid" 'white)
                (text "0" 14 'black) (square 6 "solid" 'white)
                (text "1" 14 'black) (square 6 "solid" 'white)
                (text "2" 14 'black) (square 6 "solid" 'white)
                (text "3" 14 'black) (square 6 "solid" 'white)
                (text "4" 14 'black) (square 6 "solid" 'white)
                (text "5" 14 'black) )
        400)
       (rectangle (max n 0) (max (* n 15/100) 0) "solid" 'white))
      (error "n must be positive") ))

(: board-image : Board Integer -> Image)
;; adds labels to four-quad
(define (board-image b n)
  (beside
   (above (col n)
          (beside (row n)
                  (four-quad b n))
          ;(rectangle (max n 0) (max (* n 15/100) 0)  "solid" 'white)
          )
   (rectangle (max (* n 15/100) 0) (max (* n 85/100) 0) "solid" 'white)))

(: game-image : Game Integer -> Image)
;; adds indication of next player and next action
(define (game-image g n)
  (match g
    [(Game b p a)
     (if (positive? n)
         (above (board-image b n)
                (text (string-append
                       "Next Player: "
                       (symbol->string p)
                       " ("
                       (symbol->string a)
                       ")")
                      15 'black))
         (error "n must be positive"))] ))







;; === PROJECT B


;; Rendering --------------------------------------------------------------

(: scale-to-width : Image Integer -> Image)
;; scale image to desired width
;; crediit to Adam Shaw
(define (scale-to-width img w)
  (if (<= w 0)
      (error "Width must be positive.")
      (local
        {(define scalar
           (/ w (image-width img)))}
        (if (<= scalar 0)
            (error "bug") ;; this is just to "outwit" the typechecker
            (scale scalar img)))))

(define heading
  (above
   (text "PENTAGO" 40 'darkred)
   (text "" 11 'white)
   (text "Instructions" 20 'black)
   (text "Place: enter number for row and col" 15 'black)
   (text "Twist: clockwise: j is NW, k is NE, l is SW, ; is SE" 15 'black)
   (text "counter-clockwise: a is NW, s is NE, d is SW, f is SE" 15 'black)
   (text "" 11 'white)))

(: draw : World -> Image)
;; draws game-image
(define (draw w)
  (match w
    [(World g r c p1 p2 s)
     (above
      heading
      ;(text display-name 20 'black)
      (game-image (World-game w) 400)
      (text (if (= r 10)
                "row:"
                (string-append "row: " (number->string r)))
            15 'black)
      (text (if (= c 10)
                "col:"
                (string-append "col: " (number->string c)))
            15 'black)
      (if (stop w)
          (text (if (equal? (outcome g) 'tie)
                    "Tie!"
                    (string-append 
                     "Winner: "
                     (symbol->string (outcome g)))) 15 'darkred)
          (text "" 15 'white)))]))




;; BOT --------------------------------------------------------------------

(: first-available : Game -> (Optional Move))
;; give back (Some Move) or 'none
(define (first-available g)
  (match g
    [(Game b p a)
     (if (symbol=? a 'place)
         (local
           {(: bot-place : Loc -> (Optional Move))
            ;; runs through board locs starting from 0 0 to find empty space
            (define (bot-place l)
              (match l
                [(Loc r c)
                 (if (or (> r 5) (> c 5))
                     'none
                     (cond
                       [(equal? (board-ref b (Loc r c)) 'none)
                        (Some (Move (Loc r c) 'NW 'clockwise))]
                       [(< r 5) (bot-place (Loc (add1 r) c))]
                       [else (bot-place (Loc 0 (add1 c)))]))]))}
           (bot-place (Loc 0 0)))
         (Some (Move (Loc 0 0) 'NW 'clockwise)))]))
(check-expect (first-available (Game grant 'white 'place))
              (Some (Move (Loc 1 0) 'NW 'clockwise)))
(check-expect (first-available (Game grant 'black 'twist))
              (Some (Move (Loc 0 0) 'NW 'clockwise)))

(define grant-world (World grant-game 10 10 'kay 'jay first-available))

(: tick : World -> World)
;; both bots place and twist
(define (tick w)
  (match w
    [(World g r c p1 p2 s)
     (match (s g)
       [(Some a)
        (match a
          [(Move l q d)
           (World (twist-quadrant
                   (place-marble g (Game-next-player g) l) q d)
                  r c p1 p2 s)])])]))
(check-expect
 (World-game (tick grant-world))
 (Game
  (Board
   (list 'none (Some 'white) (Some 'black) 'none 'none 'none 'none 'none 'none)
   '(none none none none none none none none none)
   '(none none none none none none none none none)
   (list 'none (Some 'white) 'none 'none 'none 'none 'none 'none 'none))
  'black
  'place))

(: white-tick : World -> World)
;; first player (white) bot places and twists 
(define (white-tick w)
  (match w
    [(World g r c p1 p2 s)
     (match (s g)
       [(Some a)
        (match a
          [(Move l q d)
           (if (equal? (Game-next-player g) 'white)
               (World (twist-quadrant
                       (place-marble g (Game-next-player g) l) q d)
                      r c p1 p2 s)
               w)])])]))
(check-expect
 (World-game (white-tick grant-world))
 (Game
  (Board
   (list 'none (Some 'white) (Some 'black) 'none 'none 'none 'none 'none 'none)
   '(none none none none none none none none none)
   '(none none none none none none none none none)
   (list 'none (Some 'white) 'none 'none 'none 'none 'none 'none 'none))
  'black
  'place))
(check-expect
 (World-game
  (white-tick
   (World (Game grant 'black 'place) 10 10 'kay 'jay first-available)))
 (Game grant 'black 'place))

(: black-tick : World -> World)
;; first player (white) bot places and twists 
(define (black-tick w)
  (match w
    [(World g r c p1 p2 s)
     (match (s g)
       [(Some a)
        (match a
          [(Move l q d)
           (if (equal? (Game-next-player g) 'black)
               (World (twist-quadrant
                       (place-marble g (Game-next-player g) l) q d)
                      r c p1 p2 s)
               w)])]
       [_ (error "bug")])]))
(check-expect
 (World-game (black-tick grant-world)) grant-game)
(check-expect
 (World-game
  (black-tick
   (World (Game grant 'black 'place) 10 10 'kay 'jay first-available)))
 (Game
  (Board
   (list 'none (Some 'black) (Some 'black) 'none 'none 'none 'none 'none 'none)
   '(none none none none none none none none none)
   '(none none none none none none none none none)
   (list 'none (Some 'white) 'none 'none 'none 'none 'none 'none 'none))
  'white
  'place))





;; HUMAN ------------------------------------------------------------------

(: str->num : String -> Integer)
;; converts key input of a string to a number
(define (str->num s)
  (match s
    ["0" 0]
    ["1" 1]
    ["2" 2]
    ["3" 3]
    ["4" 4]
    ["5" 5]
    [_ 10]))
(check-expect (str->num "0") 0)
(check-expect (str->num "3") 3)
(check-expect (str->num "d") 10)


(: key : World String -> World)
;; places marble or twists quadrent based on key pressed
(define (key w k)
  (match w
    [(World g r c p1 p2 s)
     (match g
       [(Game b p a)
        (match a
          ['place
           ;; enter r c of loc desired
           (cond
             [(= r 10) (World g (str->num k) c p1 p2 s)]
             [(= c 10) (World (place-marble g p (Loc r (str->num k)))
                              r (str->num k) p1 p2 s)]
             [else w])]
          ['twist
           (match k
             ["j" (World
                   (twist-quadrant g 'NW 'clockwise) 10 10 p1 p2 s)]
             ["k" (World
                   (twist-quadrant g 'NE 'clockwise) 10 10 p1 p2 s)]
             ["l" (World
                   (twist-quadrant g 'SW 'clockwise) 10 10 p1 p2 s)]
             [";" (World
                   (twist-quadrant g 'SE 'clockwise) 10 10 p1 p2 s)]
             ["a" (World
                   (twist-quadrant g 'NW 'counterclockwise) 10 10 p1 p2 s)]
             ["s" (World
                   (twist-quadrant g 'NE 'counterclockwise) 10 10 p1 p2 s)]
             ["d" (World
                   (twist-quadrant g 'SW 'counterclockwise) 10 10 p1 p2 s)]
             ["f" (World
                   (twist-quadrant g 'SE 'counterclockwise) 10 10 p1 p2 s)]
             [_ w])])])]))
(check-expect (World-row (key grant-world "1")) 1)
(check-expect
 (World-game (key (World grant-game 2 10 'k 'j first-available) "3"))
 (Game
  (Board
   (list (Some 'black) 'none 'none 'none 'none 'none 'none 'none 'none)
   (list 'none 'none 'none 'none 'none 'none (Some 'white) 'none 'none)
   '(none none none none none none none none none)
   (list 'none (Some 'white) 'none 'none 'none 'none 'none 'none 'none))
  'white
  'twist))
(check-expect
 (World-game
  (key (World (Game grant 'black 'twist) 10 10 'k 'j first-available) "a"))
 (Game
  (Board
   (list 'none 'none 'none 'none 'none 'none (Some 'black) 'none 'none)
   '(none none none none none none none none none)
   '(none none none none none none none none none)
   (list 'none (Some 'white) 'none 'none 'none 'none 'none 'none 'none))
  'white
  'place))



;; General Game -----------------------------------------------------------

(: stop : World -> Boolean)
;; stop when game-over is true
(define (stop w)
  (if (game-over? (World-game w)) #t #f))
(check-expect (stop grant-world) #f)
(check-expect
 (stop (World (Game jackson 'white 'place) 10 10 'kay 'jay first-available)) #t)


(: pentago : (U Human Bot) (U Human Bot) -> World)
;; creates world for two players
(define (pentago p1 p2)
  (match* (p1 p2)
    [((Bot n1 m1) (Bot n2 m2))
     (big-bang (World new-game 10 10 n1 n2 m1) : World 
               [name "Pentago"]
               [to-draw draw]
               [on-key key]
               [on-tick tick 1/5]
               [stop-when stop])]
    [((Bot n1 m1) _)
     (big-bang (World new-game 10 10 n1 p2 m1) : World 
               [name "Pentago"]
               [to-draw draw]
               [on-key key]
               [on-tick white-tick 1/5]
               [stop-when stop])]
    [(_ (Bot n2 m2))
     (big-bang (World new-game 10 10 p1 n2 m2) : World 
               [name "Pentago"]
               [to-draw draw]
               [on-key key]
               [on-tick black-tick 1/5]
               [stop-when stop])]
    [(_ _)
     (big-bang (World new-game 10 10 p1 p2 first-available) : World 
               [name "Pentago"]
               [to-draw draw]
               [on-key key]
               [stop-when stop])]))






;; === PROJECT C


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