#lang racket

;;;
;;; SEA BATTLE
;;;

(require 2htdp/image
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
               ship#         ; plaayer ship number being initialized
               ship          ; player and computer ships
               board         ; hash of player and computer boards                          ; hash of
               player-containers        ; containers for player
               computer-containers)     ; containers for computer
  #:mutable #:transparent)

(define DIM 10)
(define STATE-INIT!   0)
(define STATE-GO!     1)
(define STATE-REVEAL! 2)
(define STATE-DRAW!   3)
(define STATE-LOSE!   4)
(define STATE-WIN!    5)
(define DEBUG? #f)

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
                  (hash-ref (world-ship ws) (cons (if (world-player? ws) #t #f) n))))
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
                   (define hits (ship-hit-count ws ship))                   
                   (cond
                     ;; Ship is sunk.
                     [(= hits (vector-length ship)) "Sunk!"]
                     ;; state isn't go! or player?, show ship hits.
                     [(or (not (= (world-state ws) STATE-GO!))
                          player?) (format "~a / ~a"
                                           (vector-length ship)
                                           (ship-hit-count ws ship))]
                     ;; Nothing to report.
                     [else " "])]))
        (- (current-container-button-height) 8) 'white))

(define (ship-hit-count ws ship)
  (count (compose not false?)
         (for/list ([n ship])
           (unbox (hash-ref (world-board ws) (cons (world-player? ws) n))))))

;;;
;;; INIT
;;;


(define (toggle!)
  (define-from-struct clicker-evt (current-clicker-evt) ctn btn ws)
  (define cname (container-name ctn))
  (define bname (button-name btn))
  (define n (+ (* DIM cname) bname))
  (define t (hash-ref (world-board ws) (cons #t n)))
  (define ship (hash-ref (world-ship ws) (cons #t (world-ship# ws))))
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

(define (mark!)
  (define-from-struct clicker-evt (current-clicker-evt) ctn btn ws)
  (define cname (container-name ctn))
  (define bname (button-name btn))
  (unless (world-marked? ws)    
    (define n (+ (* DIM cname) bname))
    (define key (cons #f n))
    (define bx (hash-ref (world-board ws) key))  
    (when (false? (unbox bx))      
      (set-world-marked?! ws #t)
      (set-box! bx #t)
      (set-button-label! btn (get-button-label ws #f n))
      (when (was-a-ship-sunk? ws #f n)
        (set-world-computer-ships-left! ws (sub1 (world-computer-ships-left ws)))))))

(define (was-a-ship-sunk? ws target-is-player? n)
  ;; Get the target's ships.
  (define ships (for/list ([s# (range (length (world-ship-sizes ws)))])
                  (hash-ref (world-ship ws) (cons target-is-player? s#))))
  ;; Get the target ship containing n, or false if none found.
  (define ship (for/first ([ship ships] #:when (vector-member n ship))
                 ship))  
  (cond
    [(false? ship) #f]
    [else
     ;; Deermine if every ship slot is a hit.
     (for/and ([slot (remove n (vector->list ship))])
       (unbox (hash-ref (world-board ws) (cons target-is-player? slot)))       )]))

(define (get-button-label ws player? n)
  (define ns (for/fold ([acc empty])
                       ([s# (range (length (world-ship-sizes ws)))])
               (append (vector->list
                        (hash-ref (world-ship ws) (cons player? s#)))
                       acc)))
  (if (member n ns) HIT-LABEL MISS-LABEL))

(define (reveal!)  
  (define-from-struct clicker-evt (current-clicker-evt) ctn btn ws)
  (define cname (container-name ctn))
  (define bname (button-name btn))
  (lock-and-reveal! ws))

(define (lock-and-reveal! ws)
  (set-world-player?! ws #f)
  ;; Make computer board buttons do nothing.
  (for ([n (range DIM)])
    (set-container-up-action-active?!
     (find-container n (world-computer-containers ws))
     #f))
  ;; Reveal computer ships.
  (for([s# (range (length (world-ship-sizes ws)))])
    (define ship (hash-ref (world-ship ws) (cons #f s#)))
    (for ([n ship])      
      (unless (unbox (hash-ref (world-board ws) (cons #f n)))
        (define-values (cname bname) (quotient/remainder n DIM))
        (define btn (second(find-container/button cname bname (world-computer-containers ws))))
        (set-button-label! btn SHIP-LABEL))))
  ;; Deactivate reveal! and activate restart button for player and computer.
  (for ([ctns (list world-player-containers world-computer-containers)])
    (set-button-active?!
     (second (find-container/button 'action 'reveal! (ctns ws))) #f)
    (set-button-active?!
     (second (find-container/button 'action 'restart! (ctns ws))) #t))
  (set-world-state! ws
                      (cond
                        [(and (zero? (world-player-ships-left ws))
                              (zero? (world-computer-ships-left ws)))
                         STATE-DRAW!]
                        [(zero? (world-player-ships-left ws))
                         STATE-LOSE!]
                        [(zero? (world-computer-ships-left ws))
                         STATE-WIN!]
                        [else STATE-REVEAL!])))

(define (restart!) (init-world!))

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

(define (sq< x y)  
  (cond
    [(and (false? x) (false? y)) #t]
    [(and (number? x) (false? y)) #t]
    [(and (false? x) (number? y)) #f]
    [else (<= x y)]))

(define (set-ship!)
  (define-from-struct clicker-evt (current-clicker-evt) ctn btn ws)
  (define cname (container-name ctn))
  (define bname (button-name btn))
  (set-world-ship#! ws bname))

(define (go!)
  (define-from-struct clicker-evt (current-clicker-evt) ctn btn ws)
  (define cname (container-name ctn))
  (define bname (button-name btn))
  (define done? (for/and ([n (range (length (world-ship-sizes ws)))])
                  (define ship (hash-ref (world-ship ws) (cons #t n)))
                  (zero? (vector-count false? ship))))
  (unless (false? done?)
    (set-world-state! ws STATE-GO!)
    (set-world-player?! ws #f)
    (define ships (build-ships (range (sqr DIM)) (world-ship-sizes ws)))
    (for ([ship ships]
          [n (in-naturals)])
      (define vec (hash-ref (world-ship ws) (cons #f n)))
      (for ([val ship]
            [idx (in-naturals)])
        (vector-set! vec idx val)))    
    ;; Make player ship buttons do nothing.
    (set-container-up-action-active?!
     (find-container 'ships (world-player-containers ws))
     #f)
    (for ([bname (range (length (world-ship-sizes ws)))])
      (define btn (second (find-container/button 'ships bname (world-player-containers ws))))      
      (define lbl (button-label btn))
      (set-label-bg-color! lbl 'transparent)
      ;; Set ship board values to #f
      (define ship (hash-ref (world-ship ws) (cons #t bname)))
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
       (second (find-container/button 'action 'reveal! (ctns ws))) #t))))

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

(define (swap)
  (define-from-struct clicker-evt (current-clicker-evt) ctn btn ws)
  (cond
    [(or (not (= (world-state ws) STATE-GO!))              
         (world-player? ws)
         (not (world-marked? ws)))
     (set-world-player?! ws (not (world-player? ws)))]
    [else
     ;; Setting mark to false represents the end of a turn.
     (set-world-marked?! ws #f)
     (define n (get-board-pos ws))
     (define bx (hash-ref (world-board ws) (cons #t n)))
     (set-box! bx #t)
     (define-values (cname bname) (quotient/remainder n DIM))
     (define btn (second (find-container/button  cname bname (world-player-containers ws))))
     (set-button-label! btn (get-button-label ws #t n))
     (when (was-a-ship-sunk? ws #t n)
       (set-world-player-ships-left! ws (sub1 (world-player-ships-left ws))))    
     ;; We've completed a turn. If either side has zero ships the game is over.
     (if (or (zero? (world-player-ships-left ws))
             (zero? (world-computer-ships-left ws)))
         ;; Disable the computer board and reveal ships.
         (lock-and-reveal! ws)
         (set-world-player?! ws (not (world-player? ws))))]))

;;Produces a random board position from a pool of available
;; board positions.
(define (get-board-pos ws)
  (define pool (for/fold ([acc empty])
                         ([n (range (sqr DIM))])
                 (define bx (hash-ref (world-board ws) (cons #t n)))
                 (if (false? (unbox bx))
                     (cons n acc)
                     acc)))
  (car (shuffle pool)))

(define (create-board-hash n)
  (for*/hash ([b '(#f #t)]
              [n (range n)])
    (values (cons b n) (box #f))))

(define (create-ship-hash sizes)
  (for*/hash ([b '(#f #t)]
              [n (range (length sizes))])
    (values (cons b n) (make-vector (list-ref sizes n) #f))))

(define (create-containers player?)
  (append (create-board-containers player?)
          (create-ship-containers player?)
          (create-action-containers)))

(define (create-board-containers player?)
  (for/list ([c (range DIM)])
    (make-container (name c)
                    (y-offset (* c (current-container-button-height)))                      
                    (buttons (for/list ([b (range DIM)])                               
                               (make-button (name b)
                                            (label (make-label))
                                            (up-action
                                             (if player? toggle! mark!))))))))

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
                                           (up-action set-ship! )))))))

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

(define (init-world! (dim 10) (sizes '(5 4 3 3 2)))
  (define player? #t)
  (define marked? #f)
  (define player-ships-left (length sizes))
  (define computer-ships-left (length sizes))
  (define ship# 0)
  (world STATE-INIT!
         player?
         marked?
         sizes
         player-ships-left
         computer-ships-left
         ship#
         (create-ship-hash sizes)
         (create-board-hash (sqr dim))         
         (create-containers player?)
         (create-containers (not player?))))

(big-bang (init-world!)
  (on-mouse mouse-handler)
  (to-draw render)
  (name "SEA BATTLE"))