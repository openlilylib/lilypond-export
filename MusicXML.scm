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

(define-module (lilypond-export MusicXML))

(use-modules
 (oll-core scheme tree)
 (lilypond-export api)
 (lily))

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
           (if (and (not chord) (markup-list? lyric))
               (for-each
                (lambda (lyric)
                (writeln "<lyric><syllabic>single</syllabic><text>~A</text></lyric>" lyric)
                ) lyric))

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
                      (beamcont #f)
                      (moment-list (sort (filter ly:moment? (tree-get-keys musicexport (list measure))) ly:moment<?))
                      (first-moment (ly:make-moment 0)))
                  
                  (if (> (length moment-list) 0) (set! first-moment (car moment-list)))
                  
                  (writeln "<measure number=\"~A\">" measure)

                  (writeln "<attributes>")
                  (writeln "<divisions>~A</divisions>" divisions) ; divisions by measure?
                  (let ((meter (tree-get musicexport (list measure first-moment staff 'timesig))))
                    (if (number-pair? meter)
                        (writeln "<time><beats>~A</beats><beat-type>~A</beat-type></time>" (car meter)(cdr meter))))
                  (writeclef measure first-moment #f)
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
                          )) moment-list)
                     ) (sort (tree-get-keys grid (list staff)) (lambda (a b) (< a b))))

                  (writeln "</measure>")
                  )) (sort (filter integer? (tree-get-keys musicexport '())) (lambda (a b) (< a b))))

             (writeln "</part>")
             ) staff-list)

          (writeln "</score-partwise>")
          )))
    ))

(set-object-property! exportMusicXML 'file-suffix "xml")

