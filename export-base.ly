%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
% This file is part of openLilyLib,                                           %
%                      ===========                                            %
% the community library project for GNU LilyPond                              %
% (https://github.com/openlilylib)                                            %
%              -----------                                                    %
%                                                                             %
% Library: lilypond-export                                                    %
%          ===============                                                    %
%                                                                             %
% export foreign file formats with LilyPond                                   %
%                                                                             %
% lilypond-export is free software: you can redistribute it and/or modify     %
% it under the terms of the GNU General Public License as published by        %
% the Free Software Foundation, either version 3 of the License, or           %
% (at your option) any later version.                                         %
%                                                                             %
% lilypond-export is distributed in the hope that it will be useful,          %
% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
% GNU General Public License for more details.                                %
%                                                                             %
% You should have received a copy of the GNU General Public License           %
% along with openLilyLib. If not, see <http://www.gnu.org/licenses/>.         %
%                                                                             %
% openLilyLib is maintained by Urs Liska, ul@openlilylib.org                  %
% lilypond-export is maintained by Jan-Peter Voigt, jp.voigt@gmx.de           %
%                                                                             %
%       Copyright Jan-Peter Voigt, Urs Liska, 2017, 2018                      %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\version "2.19.58"
#(use-modules (lilypond-export api))

#(ly:warning "deprecated include \"export-base.ly\"!")
#(ly:warning "do \\include \"oll-core/package.ily\" and \"\n\\loadPackage lilypond-export\"")

%%%% export music
% filebase: file basename - suffix (.krn/.xml) is taken from the exporter
% exporter: function or symbol: hum -> humdrum, xml -> musicXML, not implemented yet: [l]mei -> [L-]MEI
% music: the music to export
#(define (symbol-or-procedure? v) (or (symbol? v)(procedure? v)))
exportMusic =
#(let ((exporters `((xml . ,exportMusicXML)(hum . ,exportHumdrum)(lily . ,exportLilyPond))))
   (define-void-function (filebase exporter music)((string? (ly:parser-output-name)) symbol-or-procedure? ly:music?)
     (if (symbol? exporter) (set! exporter (ly:assoc-get exporter exporters exportMusicXML #t)))
     (ly:run-translator (ly:score-music (scorify-music music)) (FileExport `((filebase . ,filebase)(exporter . ,exporter)) ))
     ))

