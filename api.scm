;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%                                                                             %
;% This file is part of openLilyLib,                                           %
;%                      ===========                                            %
;% the community library project for GNU LilyPond                              %
;% (https://github.com/openlilylib)                                            %
;%              -----------                                                    %
;%                                                                             %
;% Library: lilypond-export                                                    %
;%          ===============                                                    %
;%                                                                             %
;% export foreign file formats with LilyPond                                   %
;%                                                                             %
;% lilypond-export is free software: you can redistribute it and/or modify     %
;% it under the terms of the GNU General Public License as published by        %
;% the Free Software Foundation, either version 3 of the License, or           %
;% (at your option) any later version.                                         %
;%                                                                             %
;% lilypond-export is distributed in the hope that it will be useful,          %
;% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
;% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
;% GNU General Public License for more details.                                %
;%                                                                             %
;% You should have received a copy of the GNU General Public License           %
;% along with openLilyLib. If not, see <http://www.gnu.org/licenses/>.         %
;%                                                                             %
;% openLilyLib is maintained by Urs Liska, ul@openlilylib.org                  %
;% lilypond-export is maintained by Jan-Peter Voigt, jp.voigt@gmx.de           %
;%                                                                             %
;%       Copyright Jan-Peter Voigt, Urs Liska, 2017                            %
;%                                                                             %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define-module (lilypond-export api))

(use-modules
 (oll-core scheme tree)
 (lily))

; create duration from moment
(define-public (moment->duration mom)
  (ly:make-duration
   (ly:intlog2 (ly:moment-main-denominator mom)) 0
   (ly:moment-main-numerator mom)
   ))

((@@ (lily) translator-property-description) 'voice-context-count integer? "Count voice contexts")
((@@ (lily) translator-property-description) 'staff-context-count integer? "Count staff contexts")
((@@ (lily) translator-property-description) 'voice-id integer? "Voice ID")
((@@ (lily) translator-property-description) 'staff-id integer? "Staff ID")
((@@ (lily) translator-property-description) 'music-export tree? "Music export store")

; check name property of music object
(define (music-is? m n) (and (ly:music? m)(eq? n (ly:music-property m 'name))))

; combine note-events to event-chord
(define (combine-notes current music)
  (define (artic-type music) (ly:music-property music 'articulation-type))
  (if (not (equal? (ly:music-length current)(ly:music-length music)))
      (ly:warning "durations differ: ~A ~A" current music))
  (cond
   ; only take rests, if there is no music
   ((music-is? music 'RestEvent) ; (and (ly:music? current)(eq? 'RestEvent (ly:music-property music 'name)))
     (if (ly:music? current) current music))
   ; if we already have an event-chord, add note-event
   ((music-is? current 'EventChord) ;(and (ly:music? current)(eq? 'EventChord (ly:music-property current 'name)))
     (let* ((elements (ly:music-property current 'elements))
            (artics (ly:music-property music 'articulations))
            (an1 (filter symbol? (map artic-type elements))))
       ; reset articulations and place them in the event-chord
       (ly:music-set-property! music 'articulations '())
       ; add element and alement articulations to event-chord
       (ly:music-set-property! current 'elements `(,@elements ,music ,@artics))
       current
       ))
   ; if current is rest, override it with music
   ((music-is? current 'RestEvent) ; (and (ly:music? current)(eq? 'RestEvent (ly:music-property current 'name)))
     music)
   ; create event-chord with two note-events
   ((ly:music? current)
    (let* ((artics1 (ly:music-property current 'articulations))
           (artics2 (ly:music-property music 'articulations))
           (an1 (filter symbol? (map artic-type artics1))))
      (ly:music-set-property! current 'articulations '())
      (ly:music-set-property! music 'articulations '())
      (make-music 'EventChord 'elements
        `(,current ,music ,@artics1
           ,@(filter (lambda (v) (not (memv (ly:music-property v 'articulation-type) an1))) artics2))
        'duration (ly:music-property music 'duration)
        )))
   (else music))
  )


; engraver to collect all note- and rest-events
(define-public collectVoice
  (lambda (context)
    (let ((id 0)
          (beam-time '(#f . #f))
          (tuplet-time '(#f . #f)))
      (define (grob-cause grob)
        (cond
         ((ly:grob? grob) (grob-cause (ly:grob-property grob 'cause)))
         ((ly:music? grob) grob)
         (grob (grob-cause (ly:event-property grob 'music-cause)))
         (else #f)
         ))
      (make-engraver
       ((initialize trans)
        (let* ((staff-context (ly:context-find context 'Staff))
               (stvc (ly:context-property staff-context 'voice-context-count 0))) ; hole Zahl der Voices in diesem Staff
          (set! stvc (1+ stvc))
          (ly:context-set-property! staff-context 'voice-context-count stvc)
          (set! id stvc)
          (ly:context-set-property! context 'voice-id id)
          (ly:message "init Voice ~A/~A (~A)" (ly:context-property context 'staff-id) (ly:context-id context) id)
          ))
       (listeners
        ((StreamEvent engraver event) ; listen to any event
          (let ((musicexport (ly:context-property context 'music-export))
                (music (ly:event-property event 'music-cause))
                (bar (ly:context-property context 'currentBarNumber 1))
                (moment (ly:context-property context 'measurePosition (ly:make-moment 0))))
            ; notes and rests are stored in the tree under measeure/moment/staff/voice
            ; TODO MultiMeasureRests!
            (if (ly:music? music)
                (let* ((path (list bar moment
                               (ly:context-property context 'staff-id)
                               (ly:context-property context 'voice-id)))
                       (notes (tree-get musicexport path)))
                  (ly:music-set-property! music 'timestamp (cons bar moment))
                  (cond
                   ((memq (ly:music-property music 'name) '(NoteEvent RestEvent))
                    (let ((dur (ly:event-property event 'duration)))
                      ; track shortest duration (musicXML/MEI divisions)
                      (let ((shortdur (tree-get musicexport '(division-dur))))
                        (if (and (ly:duration? dur)(or (not shortdur) (ly:duration<? dur shortdur)))
                            (tree-set! musicexport '(division-dur) dur))
                        )
                      ; if we already have a note, combine it to a eventchord
                      (if (ly:music? notes) (set! music (combine-notes notes music)))
                      ; tuplets
                      (let ((scale (ly:duration-scale dur)))
                        (if (not (integer? scale))
                            (let ((num (numerator scale))
                                  (den (denominator scale)))
                              ;(ly:message "scale ~A/~A" num den)
                              (tree-set! musicexport `(,@path scale) scale)
                              )))
                      ; remember current time
                      (ly:event-set-property! event 'timestamp (cons bar moment))
                      ; track time for beams
                      (if (not (and
                                (pair? (cdr beam-time))
                                (equal? (cadr beam-time) bar)
                                (equal? (cddr beam-time) moment)))
                          (set! beam-time (cons (cdr beam-time) (cons bar moment))))
                      ; track time for tuplets
                      (if (not (and
                                (pair? (cdr tuplet-time))
                                (equal? (cadr tuplet-time) bar)
                                (equal? (cddr tuplet-time) moment)))
                          (set! tuplet-time (cons (cdr tuplet-time) (cons bar moment))))
                      ; store music
                      (tree-set! musicexport path music)))
                   ((eq? (ly:music-property music 'name) 'TupletSpanEvent)
                    (let ((timestamp (ly:music-property music 'timestamp))
                          (num (ly:music-property music 'numerator))
                          (den (ly:music-property music 'denominator))
                          (dir (ly:music-property music 'span-direction)))
                      ;(ly:message "tuplet ~A:~A ~A ~A ~A" num den timestamp (cons bar moment) dir)
                      (cond
                       ((and (= -1 dir)(integer? num)(integer? den))
                        (tree-set! musicexport `(,@path tuplet) `(start . ,(/ num den))))
                       ((= 1 dir)
                        (let ((tup-time (cdr tuplet-time)))
                          ;(ly:message "tuplet time ~A ~A" tup-time (cons bar moment))
                          (tree-set! musicexport `(,(car tup-time) ,(cdr tup-time) ,@(cddr path) tuplet) `(stop . #f))
                          ))
                       )))
                   )))
            ))
        )
       (acknowledgers
        ((stem-interface engraver grob source-engraver)
         (let ((musicexport (ly:context-property context 'music-export))
               (staff-id (ly:context-property context 'staff-id))
               (voice-id (ly:context-property context 'voice-id))
               (bar (ly:context-property context 'currentBarNumber 1))
               (moment (ly:context-property context 'measurePosition (ly:make-moment 0)))
               (cause (grob-cause grob)))
           (tree-set! musicexport (list bar moment staff-id voice-id 'stem 'grob) grob)
           ))
        )
       (end-acknowledgers
        ((beam-interface engraver grob source-engraver)
         (let ((musicexport (ly:context-property context 'music-export))
               (staff-id (ly:context-property context 'staff-id))
               (voice-id (ly:context-property context 'voice-id))
               (bar (ly:context-property context 'currentBarNumber 1))
               (moment (ly:context-property context 'measurePosition (ly:make-moment 0)))
               (cause (grob-cause grob)))
           (cond
            ((music-is? cause 'NoteEvent)
             (let ((start-timestamp (ly:music-property cause 'timestamp))
                   (end-timestamp (car beam-time)))
               (tree-set! musicexport (list (car start-timestamp) (cdr start-timestamp) staff-id voice-id 'beam) 'start)
               (tree-set! musicexport (list (car end-timestamp) (cdr end-timestamp) staff-id voice-id 'beam) 'end)
               ;(ly:message "beam ~A ~A" start-timestamp end-timestamp)
               ))
            ((music-is? cause 'BeamEvent)
             (let ((start-timestamp (ly:music-property cause 'timestamp))
                   (end-timestamp (cons bar moment)))
               (tree-set! musicexport (list (car start-timestamp) (cdr start-timestamp) staff-id voice-id 'beam) 'start)
               (tree-set! musicexport (list (car end-timestamp) (cdr end-timestamp) staff-id voice-id 'beam) 'end)
               ;(ly:message "beam ~A ~A" start-timestamp end-timestamp)
               ))
            (else (ly:message "Beam? ~A" cause))
            )
           ))
        )
       ))))

; collect lyrics
(define-public collectLyrics
  (lambda (context)
    (make-engraver
     (listeners
      ((lyric-event engraver event)
       (let ((musicexport (ly:context-property context 'music-export))
             (text (ly:event-property event 'text))
             (voice (ly:context-property context 'associatedVoiceContext)))
         (if (ly:context? voice)
             (let ((staff-id (ly:context-property voice 'staff-id))
                   (voice-id (ly:context-property voice 'voice-id))
                   (bar (ly:context-property context 'currentBarNumber 1))
                   (moment (ly:context-property context 'measurePosition (ly:make-moment 0))))
               (tree-set! musicexport (list bar moment staff-id voice-id 'lyric) text)
               )
             (ly:message "syl ~A" text))
         ))
      )
     )))


; engraver to group voices in one staff
(define-public collectStaff
  (lambda (context)
    (let ((id 0))
      (make-engraver
       ((initialize trans)
        ; TODO StaffGroup hierarchy! -> ly:context-property-where-defined !
        (let* ((parent-context (ly:context-parent context)) ; look in parent context for current staff count
                (psc (ly:context-property parent-context 'staff-context-count 0)))
          (set! psc (1+ psc))
          (ly:context-set-property! parent-context 'staff-context-count psc)
          (set! id psc)
          (ly:context-set-property! context 'staff-id id)
          (ly:message "init Staff ~A" id)
          ))
       (listeners
        ((SetProperty engraver event)
         (let ((musicexport (ly:context-property context 'music-export))
               (sym (ly:event-property event 'symbol))
               (val (ly:event-property event 'value))
               (bar (ly:context-property context 'currentBarNumber 1))
               (moment (ly:context-property context 'measurePosition (ly:make-moment 0)))
               (staff-id (ly:context-property context 'staff-id)))
           (if (memq sym '(clefGlyph clefPosition clefTransposition))
               (tree-set! musicexport (list bar moment staff-id sym) val))
           ))
        ((key-change-event engraver event)
         (let ((musicexport (ly:context-property context 'music-export))
               (music (ly:event-property event 'music-cause))
               (bar (ly:context-property context 'currentBarNumber 1))
               (moment (ly:context-property context 'measurePosition (ly:make-moment 0)))
               (staff-id (ly:context-property context 'staff-id)))
           (tree-set! musicexport (list bar moment staff-id 'keysig) music)
           ))
        ((time-signature-event engraver event)
         (let ((musicexport (ly:context-property context 'music-export))
               (bar (ly:context-property context 'currentBarNumber 1))
               (moment (ly:context-property context 'measurePosition (ly:make-moment 0)))
               (staff-id (ly:context-property context 'staff-id)))
           (tree-set! musicexport (list bar moment staff-id 'timesig)
             (cons (ly:event-property event 'numerator)(ly:event-property event 'denominator)))
           ))
        )
       ))))

; create layout with file exporter
(define-public FileExport
  (define-scheme-function (options)(list?)
    (let* ((exporter (ly:assoc-get 'exporter options exportHumdrum #f))
           (suffix (ly:assoc-get 'filesuffix options (object-property exporter 'file-suffix) #f))
           (filename (ly:assoc-get 'filename options
                       (format "~A.~A"
                         (ly:parser-output-name)
                         (if (string? suffix) suffix
                             (begin
                              (ly:input-warning (*location*) "no file suffix given!")
                              "dat"))) #f)))
      #{
        \layout {
          \context {
            \Voice
            \consists #collectVoice
          }
          \context {
            \Staff
            \consists #collectStaff
          }
          \context {
            \Lyrics
            \consists #collectLyrics
          }
          \context {
            \Score
            % engraver to export tree in foreign format (humdrum)
            \consists #(lambda (context)
                         (make-engraver
                          ((initialize trans)
                           (ly:message "init ~A: \"~A\"" (procedure-name exporter) filename)
                           (ly:context-set-property! context 'music-export (tree-create 'music-export))
                           )
                          ((finalize trans)
                           (let ((musicexport (ly:context-property context 'music-export)))
                             ; when score is finished, score is exported
                             (tree-set! musicexport '(finaltime)
                               (cons (ly:context-property context 'currentBarNumber) (ly:context-property context 'measurePosition)))
                             ;(for-each (lambda (sym) (ly:message "~A: ~A" sym (tree-get musicexport (list sym))))
                             ;  (filter symbol? (tree-get-keys musicexport '())))
                             (exporter musicexport filename)
                             ))
                          ))
          }
        }
      #})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; humdrum export

(define-public (exportHumdrum musicexport filename . options)
  ;(display musicexport)
  (let ((grid (tree-create 'grid))
        (bar-list (sort (filter integer? (tree-get-keys musicexport '())) (lambda (a b) (< a b))) )
        (finaltime (tree-get musicexport '(finaltime))))
    (define namesU '(#\c #\d #\e #\f #\g #\a #\b))
    (define namesL '(#\C #\D #\E #\F #\G #\A #\B))
    ; serialize pitch in  humdrum-format
    (define (hum-pitch p)
      (if (ly:pitch? p) ; rests don't have a pitch
          (let ((nn (ly:pitch-notename p))
                (oo (ly:pitch-octave p))
                (aa (ly:pitch-alteration p)))
            (define (hum-note ch oo)
              (if (or (= -1 oo)(= 0 oo))
                  (cons ch '())
                  (cons ch (hum-note ch (if (< oo 0) (1+ oo) (1- oo))))
                  ))
            (string-append
             (list->string (hum-note (list-ref (if (< oo 0) namesL namesU) nn) oo))
             (cond
              ((= 1/2 aa) "#")
              ((= -1/2 aa) "-")
              (else "")
              ))) "r"))
    ; list OR returns true, if one of its elements is true
    (define (lor l) (or (car l) (if (> (length l) 1) (lor (cdr l)) #f)))

    ;(ly:message "Takte: ~A" bar-list)

    ; extract all staffs and all voices

    (tree-walk musicexport '()
      (lambda (path key value)
        (if (= 4 (length path))
            (let ((staff (caddr path))
                  (voice (cadddr path)))
              (if (and (integer? staff)(integer? voice))
                  (tree-set! grid (list staff voice) #t))
              )
            )))
    ;(tree-display grid)

    ; write krn-file
    (with-output-to-file filename
      (lambda ()
        (let ((staff-list (sort (tree-get-keys grid (list)) (lambda (a b) (< b a)))))
          ; eine Zeile im humdrum Format (mit allen splines)
          (define (spline-entry callback bar moment)
            (string-join
             (map
              (lambda (staff)
                (let ((voice-list (sort (tree-get-keys grid (list staff)) (lambda (a b) (> b a)))))
                  (string-join
                   (map
                    (lambda (voice)
                      ; callback wird übergeben und für jeden spline aufgerufen
                      (callback bar moment staff voice)
                      ) voice-list) "\t" 'infix)
                  )) staff-list) "\t" 'infix)
            )

          ; init kern
          ;(display (spline-entry (lambda (b m s v) "**kern") #f #f))
          (display (string-join (map (lambda (s) "**kern") staff-list) "\t" 'infix))
          (newline)
          (display (string-join (map (lambda (s) (format "*staff~A" s)) staff-list) "\t" 'infix))
          (newline)

          ; we need to split splines to have multiple voices in one score
          (let ((vl (map (lambda (s) (length (tree-get-keys grid (list s)))) staff-list)))
            ; recursive function
            (define (spline-voice vl)
              ; every member with a voice-count > 1 is true
              (let ((spline-split (map (lambda (v) (> v 1)) vl)))
                ; create next line with extra spline ("*^")
                (define (explode li)
                  (let ((i (car li))
                        (l (cdr li)))
                    (if (> (length l) 0)
                        (set! l (explode l)))
                    (if (> i 1)
                        (cons 1 (cons (1- i) l))
                        (cons 1 l)
                        )
                    ))
                (if (lor spline-split)
                    (begin
                     (display (string-join (map (lambda (split) (if split "*^" "*")) spline-split) "\t" 'infix))
                     (newline)
                     (spline-voice (explode vl))
                     ))))
            (spline-voice vl)
            )

          ; für jeden Takt ...
          (for-each
           (lambda (bar)
             (let ((mom-list (sort (tree-get-keys musicexport (list bar)) ly:moment<?)))
               (display (spline-entry (lambda (b m s v) (format "=~A" b)) bar #f))
               (newline)

               ; für jeden Moment im Takt
               (for-each
                (lambda (moment)

                  (let ((timesigs (map (lambda (s) (tree-get musicexport (list bar moment s 'timesig))) staff-list))
                        (keysigs (map (lambda (s) (tree-get musicexport (list bar moment s 'keysig))) staff-list))
                        (clefs (map (lambda (s)
                                      (let ((clefGlyph (tree-get musicexport (list bar moment s 'clefGlyph)))
                                            (clefPosition (tree-get musicexport (list bar moment s 'clefPosition))))
                                        (and (integer? clefPosition)(string? clefGlyph))
                                        )) staff-list)))

                    (if (lor clefs)
                        (begin
                         (display
                          (spline-entry
                           (lambda (b m s v)
                             (let ((clefGlyph (tree-get musicexport (list b m s 'clefGlyph)))
                                   (clefPosition (tree-get musicexport (list b m s 'clefPosition)))
                                   (clefTransposition (tree-get musicexport (list b m s 'clefTransposition))))
                               (define (ottava ct)
                                 (cond
                                  ((and (> ct 0)(= 0 (modulo ct 7))) (string-append "^" (ottava (- ct 7))))
                                  ((and (< ct 0)(= 0 (modulo ct 7))) (string-append "v" (ottava (+ ct 7))))
                                  (else "")
                                  ))
                               ;(ly:message "clef ~A ~A ~A" clefGlyph clefPosition clefTransposition)
                               (if (and (integer? clefPosition)(string? clefGlyph))
                                   (format "*clef~A~A~A" (list-ref (string-split clefGlyph #\.) 1)
                                     (ottava clefTransposition) (+ 3 (/ clefPosition 2)))
                                   "*clefG2")
                               )) bar moment))
                         (newline)))

                    (if (lor (map number-pair? timesigs))
                        (begin
                         (display
                          (spline-entry
                           (lambda (b m s v)
                             (let ((meta (tree-get musicexport (list b m s 'timesig))))
                               (if (number-pair? meta)
                                   (format "*M~A/~A"
                                     (car meta)(cdr meta)) ""))) bar moment))
                         (newline)))

                    (if (lor (map (lambda (k) (music-is? k 'KeyChangeEvent)) keysigs))
                        (begin
                         (display
                          (spline-entry
                           (lambda (b m s v)
                             (let ((meta (tree-get musicexport (list b m s 'keysig))))
                               (if (music-is? meta 'KeyChangeEvent)
                                   (let ((pitch-alist (ly:music-property meta 'pitch-alist)))
                                     (define (acc p)
                                       (cond
                                        ((= 1/2 (cdr p)) (format "~A#" (list-ref namesU (car p))))
                                        ((= -1/2 (cdr p)) (format "~A-" (list-ref namesU (car p))))
                                        ((= 1 (cdr p)) (format "~A##" (list-ref namesU (car p))))
                                        ((= -1 (cdr p)) (format "~A--" (list-ref namesU (car p))))
                                        (else #f)))
                                     (format "*k[~A]" (string-join (filter (lambda (v) v) (map acc pitch-alist)) "" 'infix))
                                     )
                                   "*k[]"))) bar moment))
                         (newline)))

                    (display (spline-entry
                              (lambda (b m s v)
                                (let ((music (tree-get musicexport (list b m s v))))
                                  (if (ly:music? music) ; wenn Musik, setzen, sonst '.'
                                      (let ((dur (ly:music-property music 'duration))
                                            (tom (ly:music-property music 'name))
                                            (beam (tree-get musicexport (list b m s v 'beam))))
                                        ;(ly:message "scale ~A" (ly:duration-scale dur))
                                        (format "~A~A~A~A"
                                          (/ (expt 2 (ly:duration-log dur)) (ly:duration-scale dur))
                                          (cond
                                           ((= 1 (ly:duration-dot-count dur)) ".")
                                           ((= 2 (ly:duration-dot-count dur)) "..")
                                           (else ""))
                                          (cond
                                           ((eq? 'NoteEvent tom) (hum-pitch (ly:music-property music 'pitch)))
                                           ((eq? 'EventChord tom)
                                            (string-join
                                             (filter (lambda (v) v)
                                                     (map
                                                      (lambda (me) (if (eq? 'NoteEvent (ly:music-property me 'name))
                                                                       (hum-pitch (ly:music-property me 'pitch)) #f))
                                                      (ly:music-property music 'elements))) " " 'infix))
                                           (else "r")
                                           )
                                          (cond
                                           ((eq? 'start beam) "L")
                                           ((eq? 'end beam) "J")
                                           (else "")
                                           )
                                          ))
                                      "."))) bar moment))

                    (newline)
                    )) mom-list)


               )) bar-list)

          (if (equal? (ly:make-moment 0) (cdr finaltime))
              (begin
               (display (spline-entry (lambda (b m s v) (format "=~A" b)) (car finaltime) #f))(newline)
               ))
          (display (spline-entry (lambda (b m s v) "*-") #f #f))(newline)(newline)
          )
        ))
    ))

(set-object-property! exportHumdrum 'file-suffix "krn")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; musicXML export

(define (duration-factor dur)
  (*
   (/ 4 (expt 2 (ly:duration-log dur)))
   (duration-dot-factor (ly:duration-dot-count dur))
   (ly:duration-scale dur)
   ))

(define-public (exportMusicXML musicexport filename . options)
  (let ((grid (tree-create 'grid))
        (bar-list (sort (filter integer? (tree-get-keys musicexport '())) (lambda (a b) (< a b))) )
        (finaltime (tree-get musicexport '(finaltime)))
        (division-dur (tree-get musicexport '(division-dur)))
        (divisions 1))
    (define notenames '(C D E F G A B))
    (define types '(breve breve whole half quarter eighth 16th 32nd 64th 128th))
    (define (writeln x . args) (if (> (length args) 0) (apply format #t x args)(display x))(newline))
    (define (writepitch p)
      (if (ly:pitch? p)
          (let ((notename (list-ref notenames (ly:pitch-notename p)))
                (alter (* 2 (ly:pitch-alteration p)))
                (octave (+ 4 (ly:pitch-octave p))))
            (writeln "<pitch>")
            (writeln "<step>~A</step>" notename)
            (if (not (= 0 alter)) (writeln "<alter>~A</alter>" alter))
            (writeln "<octave>~A</octave>" octave)
            (writeln "</pitch>")
            ) (writeln "<rest />")))
    (define (writeduration dur moment)
      (if (ly:duration? dur)
          (let ((divlen (* (duration-factor dur) divisions))
                (divmom (* divisions 4 (ly:moment-main moment)))
                (addskew 0))
            (if (not (integer? divmom))
                (let* ((num (numerator divmom))
                       (den (denominator divmom))
                       (rest (modulo num den))
                       (div (/ (- num rest) den)))
                  ;(ly:message "mom: ~A ~A" (/ div rest) rest)
                  (set! addskew (/ rest den))
                  ))
            ;(ly:message "dur: ~A" (* divlen divisions))
            (if (not (integer? divlen))
                (let* ((len (inexact->exact divlen))
                       (num (numerator len))
                       (den (denominator len))
                       (rest (modulo num den))
                       (dur (/ (- num rest) den))
                       (adddur (+ addskew (/ rest den))))
                  (while (>= adddur 1)
                    (set! dur (1+ dur))
                    (set! adddur (1- adddur)))
                  ;(ly:message "time: ~A:~A ... ~A" num den rest)
                  (set! divlen dur)
                  ))
            (writeln "<duration>~A</duration>" divlen)
            )))
    (define (writetype dur)
      (if (ly:duration? dur)
          (writeln "<type>~A</type>" (list-ref types (+ 2 (ly:duration-log dur))))
          ))
    (define (writedots d) (if (> d 0) (begin (writeln "<dot/>")(writedots (1- d)))))
    (define (writetimemod dur)
      (if (and (ly:duration? dur) (not (integer? (ly:duration-scale dur))))
          (let ((num (numerator (ly:duration-scale dur)))
                (den (denominator (ly:duration-scale dur))))
            (writeln "<time-modification>")
            (writeln "<actual-notes>~A</actual-notes>" den)
            (writeln "<normal-notes>~A</normal-notes>" num)
            (writeln "</time-modification>")
            )))
    (define (writetuplet tuplet)
      (if (pair? tuplet)
          (begin
           (writeln "<notations>")
           (writeln "<tuplet number=\"1\" placement=\"above\" type=\"~A\" />" (car tuplet))
           (writeln "</notations>")
           )))
    (define (writemusic m staff voice . opts)
      (let ((dur (ly:music-property m 'duration))
            (chord (ly:assoc-get 'chord opts #f #f))
            (beam (ly:assoc-get 'beam opts))
            (tuplet (ly:assoc-get 'tuplet opts))
            (lyric (ly:assoc-get 'lyric opts))
            (moment (ly:assoc-get 'moment opts)))

        (case (ly:music-property m 'name)

          ((NoteEvent)
           (writeln "<note>")
           (if chord (writeln "<chord />"))
           (writepitch (ly:music-property m 'pitch))
           (writeduration dur moment)

           (writeln "<voice>~A</voice>" voice)
           (writetype dur)
           (writedots (if (ly:duration? dur) (ly:duration-dot-count dur) 0))

           (if (symbol? beam) (writeln "<beam number=\"1\">~A</beam>" beam))
           (writetimemod dur)
           (writetuplet tuplet)
           (if (and (not chord) (markup? lyric))
               (begin
                (writeln "<lyric><syllabic>single</syllabic><text>~A</text></lyric>" lyric)
                ))

           (writeln "</note>"))

          ((RestEvent)
           (writeln "<note>")
           (writeln "<rest />")
           (writeduration dur moment)

           (writeln "<voice>~A</voice>" voice)
           (writetype dur)
           (writedots (if (ly:duration? dur) (ly:duration-dot-count dur) 0))
           (writetimemod dur)
           (writetuplet tuplet)
           (writeln "</note>"))

          ((EventChord)
           (let* ((elements (ly:music-property m 'elements))
                  (notes (filter (lambda (m) (music-is? m 'NoteEvent)) elements))
                  (note-count (length notes))
                  (artics (filter (lambda (m) (not (music-is? m 'NoteEvent))) elements)))
             (if (> note-count 0) (apply writemusic (car notes) staff voice opts))
             ;(set! opts (assoc-remove! opts 'beam))
             (for-each
              (lambda (n)
                (apply writemusic n staff voice (cons '(chord . #t) opts))
                ) (cdr notes))
             ))

          )))

    (if (ly:duration? division-dur) (set! divisions (/ 64 (duration-factor division-dur))))
    (ly:message "divisions: ~A" divisions)

    (tree-walk musicexport '()
      (lambda (path key value)
        (if (= 4 (length path))
            (let ((staff (caddr path))
                  (voice (cadddr path)))
              (if (and (integer? staff)(integer? voice))
                  (tree-set! grid (list staff voice) #t))
              )
            )))

    (let ((staff-list (sort (tree-get-keys grid '()) (lambda (a b) (< a b)))))
      (with-output-to-file filename
        (lambda ()
          (writeln "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>")
          (writeln "<!DOCTYPE score-partwise PUBLIC \"-//Recordare//DTD MusicXML 3.0 Partwise//EN\" \"http://www.musicxml.org/dtds/partwise.dtd\">")
          (writeln "<score-partwise version=\"3.0\">")
          (writeln "<part-list>")
          (for-each
           (lambda (staff)
             (writeln "<score-part id=\"P~A\">" staff)
             (writeln "<part-name>Part ~A</part-name>" staff)
             (writeln "</score-part>")
             ) staff-list)
          (writeln "</part-list>")

          (for-each
           (lambda (staff)
             (define (writeclef measure moment doattr)
               (let ((clefGlyph (tree-get musicexport (list measure moment staff 'clefGlyph)))
                     (clefPosition (tree-get musicexport (list measure moment staff 'clefPosition)))
                     (clefTransposition (tree-get musicexport (list measure moment staff 'clefTransposition))))
                 (if (and (string? clefGlyph)(integer? clefPosition))
                     (begin
                      (if doattr (writeln "<attributes>"))
                      (writeln "<clef><sign>~A</sign><line>~A</line>~A</clef>"
                        (list-ref (string-split clefGlyph #\.) 1)
                        (+ 3 (/ clefPosition 2))
                        (if (and (not (= 0 clefTransposition))(= 0 (modulo clefTransposition 7)))
                            (format "<clef-octave-change>~A</clef-octave-change>" (/ clefTransposition 7))
                            ""))
                      (if doattr (writeln "</attributes>"))
                      ))))

             (writeln "<part id=\"P~A\">" staff)

             (for-each
              (lambda (measure)
                (let ((backup 0)
                      (beamcont #f))
                  (writeln "<measure number=\"~A\">" measure)

                  (writeln "<attributes>")
                  (writeln "<divisions>~A</divisions>" divisions) ; divisions by measure?
                  (let ((meter (tree-get musicexport (list measure (ly:make-moment 0) staff 'timesig))))
                    (if (number-pair? meter)
                        (writeln "<time><beats>~A</beats><beat-type>~A</beat-type></time>" (car meter)(cdr meter))))
                  (writeclef measure (ly:make-moment 0) #f)
                  (writeln "</attributes>")

                  (for-each
                   (lambda (voice)
                     (if (> backup 0) (writeln "<backup><duration>~A</duration></backup>" backup))
                     (set! backup 0)
                     (for-each
                      (lambda (moment)
                        (let ((music (tree-get musicexport (list measure moment staff voice))))
                          (if (not (equal? moment (ly:make-moment 0)))
                              (writeclef measure moment #t))
                          (if (ly:music? music)
                              (let ((dur (ly:music-property music 'duration))
                                    (beam (tree-get musicexport (list measure moment staff voice 'beam)))
                                    (tuplet (tree-get musicexport (list measure moment staff voice 'tuplet)))
                                    (lyric (tree-get musicexport (list measure moment staff voice 'lyric)))
                                    )
                                (case beam
                                  ((start) (set! beamcont 'continue))
                                  ((end) (set! beamcont #f))
                                  )

                                ; TODO staff grouping!
                                (writemusic music 1 voice
                                  `(beam . ,(cond
                                             ((eq? 'start beam) 'begin)
                                             ((symbol? beam) beam)
                                             ((symbol? beamcont) beamcont)))
                                  `(moment . ,moment)
                                  `(tuplet . ,tuplet)
                                  `(lyric . ,lyric))
                                (if (ly:duration? dur)
                                    (set! backup (+ backup (* (duration-factor dur) divisions))))
                                ))
                          )) (sort (filter ly:moment? (tree-get-keys musicexport (list measure))) ly:moment<?))
                     ) (tree-get-keys grid (list staff)))

                  (writeln "</measure>")
                  )) (sort (filter integer? (tree-get-keys musicexport '())) (lambda (a b) (< a b))))

             (writeln "</part>")
             ) staff-list)

          (writeln "</score-partwise>")
          )))
    ))

(set-object-property! exportMusicXML 'file-suffix "xml")


