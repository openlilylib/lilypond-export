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
;%       Contributions from Alex Roitman 2018                                  %
;%                                                                             %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;% TODO ties, slurs, grace notes

(define-module (lilypond-export MusicXML))

(use-modules
 (oll-core tree)
 (lilypond-export api)
 (lilypond-export sxml-to-xml)
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

    (define (write-xml sxml)
      (sxml->xml sxml)
      (newline))

    (define (writepitch p)
      (if (ly:pitch? p)
          (let ((notename (list-ref notenames (ly:pitch-notename p)))
                (alter (* 2 (ly:pitch-alteration p)))
                (octave (+ 4 (ly:pitch-octave p))))
            `(pitch
              (step ,notename)
              ,(if (not (= 0 alter)) `(alter ,alter) '())
              (octave ,octave)))
          '(rest)))
    (define (writekeyblock pitch-alt)
      ;; This is alternative to the traditional keys like Gm and F
      ;; https://usermanuals.musicxml.com/MusicXML/Content/EL-MusicXML-key.htm
      (let ((notename (list-ref notenames (car pitch-alt)))
            (alt (cdr pitch-alt)))
        `((key-step ,notename)
          (key-alter ,(* 2 alt))
          (key-accidental ,(acctext alt)))))
    (define (fifths pitch-alist)
      (let ((flats (length (filter (lambda (pa) (= -1/2 (cdr pa))) pitch-alist)))
            (sharps (length (filter (lambda (pa) (= 1/2 (cdr pa))) pitch-alist)))
            (others (length (filter (lambda (pa) (and (not (= 1/2 (cdr pa))) (not (= -1/2 (cdr pa))))) pitch-alist))))
        (cond
         ((> others 0) #f)
         ((and (> flats 0) (> sharps 0)) #f)
         ((> flats 0) (- flats))
         (else sharps))
      ))
    (define (writekey pitch-alist)
      (if pitch-alist
          (let* ((non-zero-pitch-alts (filter (lambda (p-a) (not (= 0 (cdr p-a)))) pitch-alist))
                 (fifths-val (fifths non-zero-pitch-alts)))
            `(key
              ,(if fifths-val
                   `((fifths ,fifths-val) (mode "none"))
                   (map writekeyblock non-zero-pitch-alts) ; alternative to traditional keys
                   )))
          '()))
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
            `(duration ,divlen)
            )))
    (define (writetype dur)
      (if (ly:duration? dur)
          `(type ,(list-ref types (+ 2 (ly:duration-log dur))))
          '()))
    (define (writedots d)
      (if (> d 0)
          (cons '(dot) (writedots (1- d)))
          '()))
    (define (writetimemod dur)
      (if (and (ly:duration? dur) (not (integer? (ly:duration-scale dur))))
          (let ((num (numerator (ly:duration-scale dur)))
                (den (denominator (ly:duration-scale dur))))
            `(time-modification
              (actual-notes ,den)
              (normal-notes ,num)))
          '()))
    (define (writetuplet tuplet)
      (if (pair? tuplet)
          `(tuplet (@ (number 1)
                      (placement "above")
                      (type ,(car tuplet))))
          '()))
    (define art-map ; articulations
      '((accent . accent)
        (marcato . strong-accent)
        (portato . detached-legato)
        (staccatissimo . staccatissimo)
        (staccato . staccato)
        (tenuto . tenuto)))
    (define orn-map ; ornaments
      '((reverseturn . inverted-turn)
        (mordent . mordent)
        (prall . shake)
        (trill . trill-mark)
        (turn . turn)))
    (define onot-map '((fermata . fermata))) ; other notaions
    (define picker
      (lambda (the-map)
        (lambda (atype)
          (if (null? atype)
              #f
              (let ((art-pair (assq (string->symbol atype) the-map)))
                (if art-pair (cdr art-pair) #f))))))
    (define (writearticulations art-types)
      (if art-types
          (let ((arts (filter identity (map (picker art-map) art-types)))
                (orns (filter identity (map (picker orn-map) art-types)))
                (onots (filter identity (map (picker onot-map) art-types))))
            `(
              ,(if (not (null? arts)) `(articulations ,(map list arts)) '())
              ,(if (not (null? orns)) `(ornaments ,(map list orns)) '())
              ,(map list onots)))
          '()))
    (define (writeslurs stype num)
      (if (and num (> num 0))
          `((slur (@ (number ,num)
                     (type ,stype)))
            ,(writeslurs stype (- num 1)))
          '()))
    (define (writenotations chord tuplet art-types slur-start slur-stop)
      (if (or (pair? tuplet) (and (not chord) (or art-types slur-start slur-stop)))
          `(notations
            ,(writetuplet tuplet)
            ,(if chord
                 '()
                 `(,(writearticulations art-types)
                   ,(writeslurs 'start slur-start)
                   ,(writeslurs 'stop slur-stop))))
          '()))
    (define (acctext accidental)
      (case accidental
        ((0) "natural")
        ((-1/2) "flat")
        ((1/2) "sharp")
        ((-1) "flat-flat")
        ((1) "double-sharp")
        (else "")))
    (define (writemusic m staff voice . opts)
      (let ((dur (ly:music-property m 'duration))
            (chord (ly:assoc-get 'chord opts #f #f))
            (pitch-acc (ly:assoc-get 'pitch-acc opts #f #f))
            (slur-start (ly:assoc-get 'slur-start opts #f #f))
            (slur-stop (ly:assoc-get 'slur-stop opts #f #f))
            (beam (ly:assoc-get 'beam opts))
            (tuplet (ly:assoc-get 'tuplet opts))
            (art-types (ly:assoc-get 'art-types opts #f))
            (lyrics (ly:assoc-get 'lyrics opts))
            (moment (ly:assoc-get 'moment opts)))
        ;(ly:message "-----> lyrics ~A" lyrics)
        (case (ly:music-property m 'name)

          ((NoteEvent)
           (write-xml
            `(note
              ,(if chord '(chord) '())
              ,(writepitch (ly:music-property m 'pitch))
              ,(writeduration dur moment)
              (voice ,voice)
              ,(writetype dur)
              ,(writedots (if (ly:duration? dur) (ly:duration-dot-count dur) 0))
              ,(if pitch-acc
                   (let* ((pitch (ly:music-property m 'pitch))
                          (my-p-a (filter
                                   (lambda (p-a) (and p-a (eqv? pitch (car p-a))))
                                   pitch-acc)))
                     (if (not (null? my-p-a))
                         `(accidental ,(acctext (cadar my-p-a)))
                         '()))
                   '())
              ,(if (symbol? beam)
                   `(beam (@ (number 1)) ,beam)
                   '())
              ,(writetimemod dur)
              ,(writenotations chord tuplet art-types slur-start slur-stop)
              ,(if (and (not chord) (list? lyrics))
                   (map (lambda (lyric)
                          ;(ly:message "~A" lyric)
                          `(lyric
                            (syllabic "single")
                            (text ,lyric)))
                     lyrics)
                   '())
              )))

          ((RestEvent)
           (write-xml
            `(note
              (rest)
              ,(writeduration dur moment)
              (voice ,voice)
              ,(writetype dur)
              ,(writedots (if (ly:duration? dur) (ly:duration-dot-count dur) 0))
              ,(writetimemod dur)
              ,(writenotations chord tuplet art-types slur-start slur-stop)
              )))

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
          (write-xml
           `(part-list
             ,(map (lambda (staff)
                     (let ((id (format #f "P~A" staff))
                           (part-name (format #f "Part ~A" staff)))
                       `(score-part (@ (id ,id)) (part-name ,part-name))))
                staff-list)))

          (for-each
           (lambda (staff)
             (define (writeclef measure moment doattr)
               (let ((clefGlyph (tree-get musicexport (list measure moment staff 'clefGlyph)))
                     (clefPosition (tree-get musicexport (list measure moment staff 'clefPosition)))
                     (clefTransposition (tree-get musicexport (list measure moment staff 'clefTransposition))))
                 (if (and (string? clefGlyph)(integer? clefPosition))
                     (let* ((sign (list-ref (string-split clefGlyph #\.) 1))
                            (line (+ 3 (/ clefPosition 2)))
                            (octave-change (if (and (not (= 0 clefTransposition))
                                                    (= 0 (modulo clefTransposition 7)))
                                               `(clef-octave-change ,(/ clefTransposition 7))
                                               '()))
                            (clef-tag `(clef
                                        (sign ,sign)
                                        (line ,line)
                                        ,octave-change
                                        )))
                       (if doattr `(attributes ,clef-tag) clef-tag))
                     '())))

             (writeln "<part id=\"P~A\">" staff)

             (for-each
              (lambda (measure)
                (let* ((backup 0)
                       (beamcont #f)
                       (unsorted-moments (filter ly:moment?
                                                 (tree-get-keys musicexport
                                                   (list measure))))
                       (moment-list (sort unsorted-moments ly:moment<?))
                       (first-moment (if (> (length moment-list) 0)
                                         (car moment-list)
                                         (ly:make-moment 0))))

                  (writeln "<measure number=\"~A\">" measure)

                  (write-xml
                   `(attributes
                     (divisions ,divisions) ; divisions by measure?
                     ,(writekey (tree-get musicexport (list measure first-moment staff 'key-pitch-alist)))
                     ,(let ((meter (tree-get musicexport
                                     (list measure first-moment staff 'timesig))))
                        (if (number-pair? meter)
                            `(time (beats ,(car meter)) (beat-type ,(cdr meter)))
                            '()))
                     ,(writeclef measure first-moment #f)))

                  (for-each
                   (lambda (voice)
                     (if (> backup 0)
                         (write-xml `(backup (duration ,backup))))
                     (set! backup 0)
                     (for-each
                      (lambda (moment)
                        (let ((music (tree-get musicexport (list measure moment staff voice))))
                          (if (not (equal? moment (ly:make-moment 0)))
                              (write-xml (writeclef measure moment #t)))
                          (if (ly:music? music)
                              (let ((dur (ly:music-property music 'duration))
                                    (beam (tree-get musicexport (list measure moment staff voice 'beam)))
                                    (pitch-acc (tree-get musicexport (list measure moment staff voice 'pitch-acc)))
                                    (art-types (tree-get musicexport (list measure moment staff voice 'art-types)))
                                    (slur-start (tree-get musicexport (list measure moment staff voice 'slur-start)))
                                    (slur-stop (tree-get musicexport (list measure moment staff voice 'slur-stop)))
                                    (tuplet (tree-get musicexport (list measure moment staff voice 'tuplet)))
                                    (lyrics (tree-get musicexport (list measure moment staff voice 'lyrics)))
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
                                  `(pitch-acc . ,pitch-acc)
                                  `(art-types . ,art-types)
                                  `(slur-start . ,slur-start)
                                  `(slur-stop . ,slur-stop)
                                  `(moment . ,moment)
                                  `(tuplet . ,tuplet)
                                  `(lyrics . ,lyrics))
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

