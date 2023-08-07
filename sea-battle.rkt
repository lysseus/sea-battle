#lang racket

;;;
;;; SEA BATTLE
;;;
;;; This game has 2 phases:
;;; - initial phase where player ships are set up on the board.
;;;   Player clicks board squares to initialize ship locations.
;;;   Player clicks ship to select it and initialize its location.
;;;   Player clicks Go! when finished to begin attack on computer
;;;   board.
;;; - mark!/swap turn in which the player clicks a bboard square,
;;;   then clicks swap to initiate computer's attack, followed by
;;;   another swap going back to computer's board for another player
;;;   attack. Play continues until all ships for a side are sunk or
;;;   Reveal! is clicked. When ships are sunk or revealed the player
;;;   can click Restart! to begin a new game.
;;;

(require (only-in racket/gui
                  play-sound)
         2htdp/image
         2htdp/universe
         utils/list
         utils/struct
         utils/2htdp/image
         utils/2htdp/clicker
         utils/2htdp/text)

(struct world (state         ; 0=init, 1=go, 2=reveal
               player?       ; computer=#f,player=#t
               marked?       ; player has marked a square on computer board?
               ship-sizes    ; list of ship vector slots
               player-ships-left
               computer-ships-left
               ship-hit      ; player ship that was hit 
               hits          ; computer's hits collected prior to ship sinking
               hit-pool      ; likely player board squares remaining for attack
               board-pool    ; player board squares remaining for attack
               ship#         ; plaayer ship number being initialized
               ships         ; player and computer ships
               board         ; hash of player and computer boards                          ; hash of
               player-containers        ; containers for player
               computer-containers)     ; containers for computer
  #:mutable #:transparent)

(define DIM 10)

;; Constants used by world-state:
(define STATE-INIT!   0)
(define STATE-GO!     1)
(define STATE-REVEAL! 2)
(define STATE-DRAW!   3)
(define STATE-LOSE!   4)
(define STATE-WIN!    5)

(define DEBUG? #f)

;; Sounds used by the game:
(define SOUND-PATH "sounds/")
(define SOUND-SHIP-HIT
  (string-append SOUND-PATH "204171__pcruzn__another-bomb-modal.WAV"))
(define SOUND-SHIP-MISSED
  (string-append SOUND-PATH "470084__sheyvan__underwater-deep-impact.wav"))
(define SOUND-SHIP-SUNK
  (string-append SOUND-PATH "96742__robinhood76__01650-underwater-bubbles-05.wav"))
(define SOUND-SHIP-SONAR
  (string-append SOUND-PATH "493162__breviceps__submarine-sonar.wav"))
(define SOUND-SHIP-RADAR
  (string-append SOUND-PATH "449672__matrixxx__radar-02-sec.wav"))
(define SOUND-SHIP-ALARM
  (string-append SOUND-PATH "104882__cgeffex__submarie-dive-horn_better.wav"))

;;;
;;; CLICKER ELEMENTS
;;;


;; Mouse events are handled by the clicke library according to the
;; definitions described by containers.
(define (mouse-handler ws x y evt)
  (process-containers ((if (world-player? ws)
                           world-player-containers
                           world-computer-containers) ws) ws x y evt)
  (define ans (current-clicker-evt))
  (if (and (clicker-evt? ans)
           (world? (clicker-evt-result ans)))
      (clicker-evt-result ans)
      ws))

;; Label defaults
(current-label-name " ")
(current-label-border? #t)
(current-label-font-color 'white)
(current-label-bg-color 'blue)
(current-label-bo-color 'gold)
(current-label-padding 8)

;; Container defaults
(current-container-button-width 30)
(current-container-button-height 30)

(define BLANK-LABEL (make-label (bg-color 'blue)))
(define SHIP-LABEL (make-label (bg-color 'lightgray)))
(define MISS-LABEL (make-label (bg-color 'white)))
(define HIT-LABEL (make-label (bg-color 'lightred)))

;;;
;;; RENDERING
;;;

(define BOARD-WIDTH (* DIM (current-container-button-width)))
(define BOARD-HEIGHT (* DIM (current-container-button-height)))
(define BAR-WIDTH 4)
(define MT-WIDTH  (+ BAR-WIDTH (* 2 BOARD-WIDTH)))
(define MT-HEIGHT BOARD-HEIGHT)
(define MT (empty-scene MT-WIDTH MT-HEIGHT 'black))
(define BAR (rectangle BAR-WIDTH MT-HEIGHT 'solid 'white))

(define side 10)
(define sq (square side 'solid 'white))
(define r (rectangle side (quotient side 2) 'solid 'white))
(define rt (right-triangle side side 'solid 'white))
(define s1 (beside (rotate 180 rt) sq sq sq sq sq sq sq sq sq (rotate -90 rt)))
(define s2 (above (beside/align 'bottom (rotate 90 r) r (rotate 90 r))
                  (beside (rotate 180 rt) sq sq sq sq sq sq sq sq (rotate -90 rt))))
(define s3 (above (beside/align 'bottom (rotate 90 r) r )
                  (beside (rotate 180 rt) sq sq sq sq sq sq sq (rotate -90 rt))))
(define s4 (above (beside/align 'bottom (rotate 90 r) r )
                  (rotate 180 (beside (rotate 180 rt) sq sq sq sq sq sq sq (rotate -90 rt)))))
(define s5 (above (beside/align 'bottom (rotate 90 r) r )
                  (beside (rotate 180 rt) sq sq sq sq sq sq (rotate -90 rt))))

(define box1 (rectangle (* 10 side) (* 5 side) 'solid 'transparent))
(define SHIP1 (overlay s1 box1))
(define SHIP2 (overlay s2 box1))
(define SHIP3 (overlay s3 box1))
(define SHIP4 (overlay s4 box1))
(define SHIP5 (overlay s5 box1))

(define BANNER-FRAME (rectangle (* 8 (current-container-button-width))
                                (current-container-button-height)
                                'solid
                                'transparent))
(define FONT-SIZE (quotient (current-container-button-height) 2))
(define PLAYER-BANNER (overlay (text "PLAYER" FONT-SIZE 'white)
                               BANNER-FRAME))
(define PLAYER-DRAWS-BANNER (overlay (text "PLAYER DRAWS" FONT-SIZE 'white)
                                     BANNER-FRAME))
(define PLAYER-LOSES-BANNER (overlay (text "PLAYER LOSES" FONT-SIZE 'white)
                                     BANNER-FRAME))
(define PLAYER-WINS-BANNER (overlay (text "PLAYER WINS" FONT-SIZE 'white)
                                    BANNER-FRAME))
(define COMPUTER-BANNER (overlay (text "COMPUTER" FONT-SIZE 'white)
                                 BANNER-FRAME))
(define COMPUTER-DRAWS-BANNER (overlay (text "COMPUTER DRAWS" FONT-SIZE 'white)
                                       BANNER-FRAME))
(define COMPUTER-LOSES-BANNER (overlay (text "COMPUTER LOSES" FONT-SIZE 'white)
                                       BANNER-FRAME))
(define COMPUTER-WINS-BANNER (overlay (text "COMPUTER WINS" FONT-SIZE 'white)
                                      BANNER-FRAME))

(define (render ws)
  (place-images/loc
   (list (image/loc BAR (posn BOARD-WIDTH 0) (place 'left 'top))
         (image/loc (if (world-player? ws)
                        (cond
                          [(= (world-state ws) STATE-WIN!) PLAYER-WINS-BANNER]
                          [(= (world-state ws) STATE-LOSE!) PLAYER-LOSES-BANNER]
                          [(= (world-state ws) STATE-DRAW!) PLAYER-DRAWS-BANNER]
                          [else PLAYER-BANNER])                        
                        (cond
                          [(= (world-state ws) STATE-WIN!) COMPUTER-LOSES-BANNER]
                          [(= (world-state ws) STATE-LOSE!) COMPUTER-WINS-BANNER]
                          [(= (world-state ws) STATE-DRAW!) COMPUTER-DRAWS-BANNER]
                          [else COMPUTER-BANNER]))
                    (posn (+ BAR-WIDTH BOARD-WIDTH (quotient BOARD-WIDTH DIM))
                          (* 1 (current-container-button-height)))
                    (place 'left 'top))
         (image/loc (draw-ships-status ws)
                    (posn (+ BAR-WIDTH BOARD-WIDTH (* 5 (current-container-button-width)))
                          (* 3 (current-container-button-height)))
                    (place 'left 'top)))
   (place-containers ((if (world-player? ws)
                          world-player-containers
                          world-computer-containers) ws)
                     MT)))

(define (draw-ships-status ws)
  (define ships (for/list ([n (range (length (world-ship-sizes ws)))])
                  (hash-ref (world-ships ws) (cons (if (world-player? ws) #t #f) n))))
  (apply above
         (for/list ([ship ships])
           (overlay (ship-status-image ws (world-player? ws) ship)
                    (rectangle (* 4 (current-container-button-width))
                               (current-container-button-height)
                               'outline
                               'white)))))

(define (ship-status-image ws player? ship)
  (text (format "~a"
                (cond
                  ;; Initializing player ships.
                  [(= (world-state ws) STATE-INIT!)
                   (format "~a / ~a"
                           (vector-length ship)
                           (vector-count natural? ship))]
                  [else
                   (define hits (ship-hit-count (world-board ws)
                                                (world-player? ws) ship))                   
                   (cond
                     ;; Ship is sunk.
                     [(= hits (vector-length ship)) "Sunk!"]
                     ;; state isn't go! or player?, show ship hits.
                     [(or (not (= (world-state ws) STATE-GO!))
                          player?) (format "~a / ~a"
                                           (vector-length ship)
                                           (ship-hit-count (world-board ws)
                                                           (world-player? ws) ship))]
                     ;; Nothing to report.
                     [else " "])]))
        (- (current-container-button-height) 8) 'white))

;;;
;;; INIT
;;;

;; Triggered by click event on player's board. Used to select/deselect
;; player ships during initial setup.
(define (toggle!)
  (define-from-struct clicker-evt (current-clicker-evt) ctn btn ws)
  (define cname (container-name ctn))
  (define bname (button-name btn))
  (define n (+ (* DIM cname) bname))
  (define t (hash-ref (world-board ws) (cons #t n)))
  (define ship (hash-ref (world-ships ws) (cons #t (world-ship# ws))))
  (cond
    [(false? (unbox t))     
     (define idx (valid-ship-idx n ship))     
     (unless (false? idx)
       (vector-set! ship idx n)
       (set-box! (hash-ref (world-board ws) (cons #t n)) #t)
       (set-button-label! btn SHIP-LABEL)
       (vector-sort! ship sq<))]
    [else
     (define idx (vector-member n ship))
     (unless (false? idx)
       (vector-set! ship idx #f)
       (set-box! (hash-ref (world-board ws) (cons #t n)) #f)
       (set-button-label! btn BLANK-LABEL)
       (vector-sort! ship sq<))]))

;; Triggered by a click evenEt on the computer's baord. Used to select
;; a computer board square for bombing. 
(define (mark!)
  (define-from-struct clicker-evt (current-clicker-evt) ctn btn ws)
  (define cname (container-name ctn))
  (define bname (button-name btn))
  (unless (world-marked? ws)
    ;; Compute the player board postion.
    (define n (+ (* DIM cname) bname))
    (define key (cons #f n))
    (define bx (hash-ref (world-board ws) key))
    ;; Only mark teh board position as hit if it wasn't preveiously.
    (when (false? (unbox bx))
      ;; Indicats the begiinning of a turn.
      (set-world-marked?! ws #t)
      (set-box! bx #t)
      ;; Save the computer ship hit, if any.
      (set-world-ship-hit! ws (get-ship-hit ws #f n))
      ;; Set the button label appropriately.
      (set-button-label! btn (get-button-label ws #f n))
      (cond
        [(false? (world-ship-hit ws)) (play-sound SOUND-SHIP-MISSED #t)]
        [else
         (play-sound SOUND-SHIP-HIT #t)
         (when (ship-sunk? (world-board ws) #f
                           (remove n (vector->list (world-ship-hit ws))))
           (play-sound SOUND-SHIP-SUNK #t)
           (set-world-computer-ships-left! ws (sub1 (world-computer-ships-left ws))))]))))

;; Returns true if the ship is sunk; otherwise false.
(define (ship-sunk? board player-ship? ship)
  (for/and ([n ship])
    (unbox (hash-ref board (cons player-ship? n)))       ))

;; Returns the count of hits for the ship. 
(define (ship-hit-count board player-ship? ship)
  (count (compose not false?)
         (for/list ([n ship])
           (unbox (hash-ref board (cons player-ship? n))))))

;; Returns the appropriate button label as the result of a bombing.
(define (get-button-label ws player? n)
  (define ns (for/fold ([acc empty])
                       ([s# (range (length (world-ship-sizes ws)))])
               (append (vector->list
                        (hash-ref (world-ships ws) (cons player? s#)))
                       acc)))
  (if (member n ns) HIT-LABEL MISS-LABEL))

;; When Reveal! button is clicked we:
;; - Deactivate action handling for the computer board.
;; - Reveal any hidden computer ships.
;; - Deactivate Reveal! button and activate Restart!
;; - Set to display the computer's board.
(define (reveal!)  
  (define-from-struct clicker-evt (current-clicker-evt) ctn btn ws)
  (lock-and-reveal! ws)
  (set-world-player?! ws #f))

;; Used to deactivate up-action clicks on the board indicated
;; by player?. Once this is done the player can no longer interact
;; with the specified board.
(define (lock-board! ws player?)
  ;; Make this board's buttons do nothing.
  (for ([n (range DIM)])
    (set-container-up-action-active?!
     (find-container n ((if player?
                            world-player-containers
                            world-computer-containers) ws))
     #f)))

;; Reveals any hidden computer ship squares. 
(define (reveal-computer-board! ws)  
  (for([s# (range (length (world-ship-sizes ws)))])
    (define ship (hash-ref (world-ships ws) (cons #f s#)))
    (for ([n ship])      
      (unless (unbox (hash-ref (world-board ws) (cons #f n)))
        (define-values (cname bname) (quotient/remainder n DIM))
        (define btn (second(find-container/button cname bname (world-computer-containers ws))))
        (set-button-label! btn SHIP-LABEL)))))

;; Deactivate Reveal! and activate Restart! button for player and computer.
(define (set-action-to-restart! ws)
  (for ([ctns (list world-player-containers world-computer-containers)])
    (set-button-active?!
     (second (find-container/button 'action 'reveal! (ctns ws))) #f)
    (set-button-active?!
     (second (find-container/button 'action 'restart! (ctns ws))) #t)))

;; Locks the computer board and reveals hidden computer ships.
(define (lock-and-reveal! ws)
  (lock-board! ws #f)
  (reveal-computer-board! ws)
  (set-action-to-restart! ws)
  (set-world-state! ws STATE-REVEAL!))

;; Triggered when Restart! button is clicked. Initiates a new game.
(define (restart!) (init-world!))

;; Returns true if the board location n is valid for placing
;; the ship.
(define (valid-ship-idx n ship)
  (define result (cond
                   ;; The ship has full.
                   [(zero? (vector-count false? ship)) #f]
                   ;; Ship is empty.
                   [(zero? (vector-count natural? ship)) #t]
                   [else
                    ;; Create a list of ship squares.
                    (define vals (filter number? (vector->list ship)))
                    (define min-val (apply min vals))
                    (define max-val (apply max vals))
                    (define-values (min-c min-b) (quotient/remainder min-val DIM))
                    (define-values (max-c max-b) (quotient/remainder max-val DIM))
                    (define-values (n-c n-b) (quotient/remainder n DIM))                    
                    (define new-vals (cons n vals))
                    (define new-min-vals (apply min new-vals))
                    (define new-max-vals (apply max new-vals))
                    (define-values (new-min-c new-min-b) (quotient/remainder new-min-vals DIM))
                    (define-values (new-max-c new-max-b) (quotient/remainder new-max-vals DIM))
                    (cond
                      [(= min-c max-c n-c)                       
                       (< (- new-max-b new-min-b) (vector-length ship))]
                      [(= min-b max-b n-b)
                       (< (- new-max-c new-min-c) (vector-length ship))]
                      [else #f])]))
  ;; Return false if we can't add the ship,
  ;; otherwise return the index of the first open slot.
  (if (false? result) #f (vector-member #f ship)))

;; Sort function for ordering the location indices of a ship.
(define (sq< x y)  
  (cond
    [(and (false? x) (false? y)) #t]
    [(and (number? x) (false? y)) #t]
    [(and (false? x) (number? y)) #f]
    [else (<= x y)]))

;; Triggered by up-action click on a player ship button.
;; Selects the player ship to be placed on the board.
(define (select-player-ship#!)
  (define-from-struct clicker-evt (current-clicker-evt) ctn btn ws)
  (define cname (container-name ctn))
  (define bname (button-name btn))
  (set-world-ship#! ws bname))

;; Triggered by up-action click event on the Go! button.
;; Evaluates whether all player ships have been placed on the
;; board and then initiates game play by presenting player with
;; the computer's board for bombing selection.
(define (go!)
  (define-from-struct clicker-evt (current-clicker-evt) ctn btn ws)
  (define cname (container-name ctn))
  (define bname (button-name btn))
  ;; Determine if all ships have been placed on the player's baord.
  (define done? (for/and ([n (range (length (world-ship-sizes ws)))])
                  (define ship (hash-ref (world-ships ws) (cons #t n)))
                  (zero? (vector-count false? ship))))
  (when done?
    (set-world-state! ws STATE-GO!)
    (set-world-player?! ws #f)
    ;; Populate computer ships.
    (define ships (build-ships (range (sqr DIM)) (world-ship-sizes ws)))
    (for ([ship ships]
          [n (in-naturals)])
      (define vec (hash-ref (world-ships ws) (cons #f n)))
      (for ([val ship]
            [idx (in-naturals)])
        (vector-set! vec idx val)))
    ;; Make player board buttons do nohing.
    (lock-board! ws #t)
    ;; Make player ship buttons do nothing.
    (set-container-up-action-active?!
     (find-container 'ships (world-player-containers ws))
     #f)
    ;; Change the player ships' label color to indicate inactive up-action.
    (for ([bname (range (length (world-ship-sizes ws)))])
      (define btn (second (find-container/button 'ships bname (world-player-containers ws))))      
      (define lbl (button-label btn))
      (set-label-bg-color! lbl 'transparent)
      ;; Set ship board values to #f
      (define ship (hash-ref (world-ships ws) (cons #t bname)))
      (for ([n ship])
        (define bx (hash-ref (world-board ws) (cons #t n)))
        (set-box! bx #f)))
    ;; Deactivate go! and activate reveal! button for player and computer.
    (for ([ctns (list world-player-containers world-computer-containers)])
      (set-button-active?!
       (second (find-container/button 'action 'go! (ctns ws))) #f)
      (set-button-active?!
       (second (find-container/button 'action 'swap (ctns ws))) #t)
      (set-button-active?!
       (second (find-container/button 'action 'reveal! (ctns ws))) #t))
    (play-sound SOUND-SHIP-RADAR #t)
    (play-sound SOUND-SHIP-ALARM #t)))

;; Creates a list of computer ships initialized with board postions.
(define (build-ships board-vals ship-lengths)
  (define-values (ships pool used)
    (for/fold ([ships empty]
               [pool board-vals]
               [used empty])
              ([len ship-lengths])
      (define ship (build-ship len pool used))
      (values (cons ship ships) (remove* ship pool) (append ship used))))
  (when DEBUG? (printf "ships=~a~%" ships))
  (map (λ (ship) (apply vector ship)) (reverse ships)))

;; Builds a single computer ship.
(define (build-ship len pool used)
  (define start (car (shuffle pool)))   
  (define step (car (shuffle `(1 ,DIM))))
  (define ship (range/length len start step))  
  (cond
    [(< (length pool) len) (error "Avaliable pool smaller than required ship length.")]
    [(and  (valid-ship? ship) 
           (for/and ([n ship]) (not (member n used))))
     ship]
    [else (build-ship len pool used)]))

;; Returns true if the ship is valid in terms of its
;; sequential board locations.
(define (valid-ship? ship)
  (define ns (if (vector? ship) (vector->list ship) ship))
  (define-values (rs cs)
    (for/fold ([rs empty] [cs empty])
              ([n (sort ns <)])
      (define-values (r c) (quotient/remainder n DIM))
      (values (cons r rs) (cons c cs))))
  (define rs=? (apply = rs))
  (define cs=? (apply = cs))
  (cond
    [(< (length (remove-duplicates ns)) (length ns)) #f]
    [(false? (or rs=? cs=?)) #f]
    [else
     (define vs (let ([tmp (if (false? rs=?) rs cs)])
                  (cons (add1 (car tmp)) tmp)))
     (define xs (drop-right vs 1))
     (define ys (drop vs 1))
     (and (for/and ([x xs]
                    [y ys])
            (zero? (sub1 (- x y))))
          (< (last ship) (sqr DIM)))]))

;; Triggered by click event on Swap button.
;; Used to swap between views of player and computer
;; board/ship display. Will initiate computer bombing
;; when player has marked a square on the computer's
;; board.
;;
;; A single turn consists of a mark/swap. The player
;; goes first, followed by the computer. 
(define (swap)
  (define-from-struct clicker-evt (current-clicker-evt) ctn btn ws)
  (cond
    ;; We're complting a turn.
    [(world-marked? ws)
     ;; Setting mark to false represents the end of a turn.
     (set-world-marked?! ws #f)
     ;; Get a board position.
     (define n (get-board-pos ws))     
     ;; Get the box for player's board position.
     (define bx (hash-ref (world-board ws) (cons #t n)))
     ;; Mark the board at our selected position as hit.
     (set-box! bx #t)
     ;; Remove the board position from the computer's board selection pool.
     (set-world-board-pool! ws (remove n (world-board-pool ws)))
     ;; Remove n from hit-pool.
     (unless (empty? (world-hit-pool ws))
       (set-world-hit-pool! ws (remove n (world-hit-pool ws))))
     ;; Determine if the ooard postion hit was a a player's ship.
     (set-world-ship-hit! ws (get-ship-hit ws #t n))
     (when DEBUG? (printf "ship-hit=~a~%" (world-ship-hit ws)))
     ;; Set the appropriate button label for the board position.
     (define-values (cname bname) (quotient/remainder n DIM))
     (define btn (second (find-container/button  cname bname (world-player-containers ws))))     
     (set-button-label! btn (get-button-label ws #t n))
     ;; Play appropriate sound and if requireed, decrement ships left.
     (cond
       [(world-ship-hit ws)
        (define Δship (remove n (vector->list (world-ship-hit ws))))                
        (play-sound SOUND-SHIP-HIT #t)
        (cond
          [(ship-sunk? (world-board ws) #t Δship)
           (play-sound SOUND-SHIP-SUNK #t)
           (set-world-hits! ws empty)
           (set-world-hit-pool! ws empty)
           (set-world-player-ships-left! ws (sub1 (world-player-ships-left ws)))]
          [else           
           ;; Normalize the hits pool for the next shot.
           (define hit-pool (normalize-hit-pool n
                                                (world-hits ws)
                                                (get-hit-pool ws n)
                                                (world-hit-pool ws)))
           (when DEBUG? (printf "hit-pool=~a~%" hit-pool))
           ;; Set the new hits pool.
           (set-world-hit-pool! ws hit-pool)
           ;; Add n to hits.
           (set-world-hits! ws (cons n (world-hits ws)))])]       
       [else (play-sound SOUND-SHIP-MISSED #t)])    
     ;; We've completed a turn. If either side has zero ships the game is over.
     (cond
       [(and (zero? (world-computer-ships-left ws))
             (zero? (world-player-ships-left ws))             )
         (lock-and-reveal! ws)
         (set-world-state! ws STATE-DRAW!)
         (set-world-player?! ws #t)]
       [(zero? (world-computer-ships-left ws))
         (lock-and-reveal! ws)
         (set-world-state! ws STATE-WIN!)
         (set-world-player?! ws #t)]
       [(zero? (world-player-ships-left ws))
         (lock-and-reveal! ws)
         (set-world-state! ws STATE-LOSE!)
         (set-world-player?! ws #f)]
       [else (set-world-player?! ws (not (world-player? ws)))])]
    [else (set-world-player?! ws (not (world-player? ws)))]))

;; Remove extraneous potentials from hit-pool
(define (normalize-hit-pool n hits n-pool prev-pool)
  (when DEBUG?
    (printf "n=~a hits=~a n-pool=~a prev-pool=~a~%" n
            hits n-pool prev-pool))
  (cond
    [(empty? prev-pool) n-pool]
    [else
     (define-values (n-r n-c) (quotient/remainder n DIM))
     (define-values (p-r p-c) (quotient/remainder (car hits) DIM))
     (when DEBUG? (printf "n-r=~a n-c=~a p-r=~a p-c=~a~%" n-r n-c p-r p-c))
     (define r (if (= n-r p-r) n-r #f))
     (define c (if (= n-c p-c) n-c #f))
     (when DEBUG? (printf "r=~a c=~a~%" r c))
     (for/fold ([acc empty])
               ([v (append n-pool prev-pool)])
       (define-values (v-r v-c) (quotient/remainder v DIM))
       (cond
         [(and r (= r v-r)) (cons v acc)]
         [(and c (= c v-c)) (cons v acc)]
         [else acc]))]))

;;Produces a random board position from a pool of available
;; board positions.
(define (get-board-pos ws)  
  (cond
    [(empty? (world-hit-pool ws))
     (when DEBUG? (printf "choosing from pool~%"))
     (car (world-board-pool ws))]
    [else
     (when DEBUG? (printf "choosing from hit-pool ~a~%" (world-hit-pool ws)))
     (car (world-hit-pool ws))]))

;; Returns the ship that was hit, either player or computer's,
;; or false if no ship was hit.
(define (get-ship-hit ws target-is-player? n)
  (define ships (for/list ([s# (range (length (world-ship-sizes ws)))])
                  (hash-ref (world-ships ws) (cons target-is-player? s#))))
  ;; Get the target ship containing n, or false if none found.
  (for/first ([ship ships] #:when (vector-member n ship)) ship))

;; Creates a pool of candiates for computer bombing selection
;; based on the player board location and board-pool. The hit-pool
;; is limited to vertical and horizontal postions adjacent to n.
(define (get-hit-pool ws n)
  (define-values (c r) (quotient/remainder n DIM))
  (for/fold ([acc empty])
            ([Δn (list (- DIM) -1 1 DIM)])
    (define v (+ n Δn))
    (define-values (Δc Δr) (quotient/remainder v DIM))
    (cond
      [(and (or (= c Δc)
                (= r Δr))
            (member v (world-board-pool ws)))
       (cons v acc)]
      [else acc])))

;; Creates a hash table of player and computer boards.
(define (create-board-hash n)
  (for*/hash ([b '(#f #t)]
              [n (range n)])
    (values (cons b n) (box #f))))

;; Creates a hash table of player and computer ships.
(define (create-ship-hash sizes)
  (for*/hash ([b '(#f #t)]
              [n (range (length sizes))])
    (values (cons b n) (make-vector (list-ref sizes n) #f))))

;; Creates the clicker library containers.
(define (create-containers player?)
  (append (create-board-containers player?)
          (create-ship-containers player?)
          (create-action-containers)))

;; Creates the clicker containers representing board buttons.
(define (create-board-containers player?)
  (for/list ([c (range DIM)])
    (make-container (name c)
                    (y-offset (* c (current-container-button-height)))                      
                    (buttons (for/list ([b (range DIM)])                               
                               (make-button (name b)
                                            (label (make-label))
                                            (up-action
                                             (if player? toggle! mark!))))))))

;; Creates the clicker containers representing ship buttons.
(define (create-ship-containers player?)
  (list
   (make-container (name 'ships)
                   (up-action-active? (if player? #t #f))
                   (x-offset (+ BAR-WIDTH BOARD-WIDTH (quotient BOARD-WIDTH DIM)))
                   (y-offset (* 3 (current-container-button-height)))
                   (button-width (* 4 (current-container-button-width)))
                   (buttons-vertical? #t)
                   (buttons (for/list ([img (list SHIP1 SHIP2 SHIP3 SHIP4 SHIP5)]
                                       [b (in-naturals)])
                              (make-button (name b)
                                           (label (make-label
                                                   (name img)
                                                   (bg-color
                                                    (if player? 'blue 'transparent))
                                                   (padding 1)))
                                           (up-action select-player-ship#! )))))))

;; Creates the clicker containers for action buttons
;; (Go! Swap Reveal! Restart!)
(define (create-action-containers)
  (list
   (make-container (name 'action)
                   (x-offset (+ BAR-WIDTH BOARD-WIDTH (quotient BOARD-WIDTH DIM)))
                   (y-offset (* 8 (current-container-button-height)))
                   (button-width (* 4 (current-container-button-width)))
                   (buttons-vertical? #f)
                   (buttons (list
                             (make-button (name 'go!)
                                          (label (make-label
                                                  (name "Go!")
                                                  (padding 8)))
                                          (up-action go!))
                             (make-button (name 'swap)
                                          (active? #f)
                                          (label (make-label
                                                  (name "Swap")
                                                  (padding 8)))
                                          (up-action swap))
                             (make-button (name 'reveal!)
                                          (active? #f)
                                          (label (make-label
                                                  (name "Reveal!")
                                                  (padding 8)))
                                          (up-action reveal!))
                             (make-button (name 'restart!)
                                          (active? #f)
                                          (label (make-label
                                                  (name "Restart!")
                                                  (padding 8)))
                                          (up-action restart!)))))))

;; Initialzie the world state.
;; This funcion is also called to begin a new game when Restart! is clicked.
(define (init-world! (dim 10) (sizes '(5 4 3 3 2)))
  (define player? #t)
  (define marked? #f)
  (define player-ships-left (length sizes))
  (define computer-ships-left (length sizes))
  (define ship-hit #f)
  (define hits empty) 
  (define hit-pool empty)
  (define board-pool (shuffle (range (sqr DIM))))
  (define ship# 0)
  (world STATE-INIT!
         player?
         marked?
         sizes
         player-ships-left
         computer-ships-left
         ship-hit
         hits
         hit-pool
         board-pool
         ship#
         (create-ship-hash sizes)
         (create-board-hash (sqr dim))         
         (create-containers player?)
         (create-containers (not player?))))

(big-bang (init-world!)
  (on-mouse mouse-handler)
  (to-draw render)
  (name "SEA BATTLE"))