;; Part A
;; Karen Xu
;; University of Chicago

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




(test)

(provide (all-defined-out))