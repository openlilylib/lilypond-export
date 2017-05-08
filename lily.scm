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

(define-module (lilypond-export lily))

(use-modules
 (oll-core scheme tree)
 (lilypond-export api)
 (lily))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; humdrum export

(define-public (exportLilyPond musicexport filename . options)
  (ly:message "[WIP] not implemented yet!")
  (let ((grid (tree-create 'grid))
        (bar-list (sort (filter integer? (tree-get-keys musicexport '())) (lambda (a b) (< a b))) )
        )
    (tree-walk musicexport '()
      (lambda (path key value)
        (if (= 4 (length path))
            (let ((staff (caddr path))
                  (voice (cadddr path)))
              (if (and (integer? staff)(integer? voice))
                  (tree-set! grid (list staff voice) #t))
              )
            )))
    (with-output-to-file filename
      (lambda ()
        (let ((staff-list (sort (tree-get-keys grid (list)) (lambda (a b) (< b a)))))
          (format #t "\\version \"~A\"" (lilypond-version))(newline)(newline)

          (for-each
           (lambda (staff)
             (let ((voice-list (sort (tree-get-keys grid (list staff)) (lambda (a b) (< b a)))))

               (for-each
                (lambda (voice)
                  (let ((x 0)) ; dummy

                    (format #t "staff.~A.voice.~A = {" staff voice)(newline)

                    (for-each
                     (lambda (bar)
                       (let ((mom-list (sort (tree-get-keys musicexport (list bar)) ly:moment<?)))

                         (for-each
                          (lambda (moment)
                            (let ((music (tree-get musicexport (list bar moment staff voice))))
                              (if (ly:music? music)
                                  (display (with-output-to-string (lambda () (display-lily-music music)))))
                              )) mom-list)

                         )) bar-list)

                    (display "}")(newline)

                    )) voice-list)

               )) staff-list)

          )))
    ))

(set-object-property! exportLilyPond 'file-suffix "export.ly")
