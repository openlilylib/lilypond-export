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

;% TODO ties, slurs

(define-module (lilypond-export api))

(use-modules
 (oll-core scheme tree)
 (lilypond-export lily)
 (lilypond-export MusicXML)
 (lilypond-export Humdrum)
 (lily))

(re-export exportLilyPond)
(re-export exportMusicXML)
(re-export exportHumdrum)

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
(define-public (music-is? m n) (and (ly:music? m)(eq? n (ly:music-property m 'name))))

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
            ; TODO MultiMeasureRests, Upbeats
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
                         (ly:assoc-get 'filebase options (ly:parser-output-name) #f)
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

