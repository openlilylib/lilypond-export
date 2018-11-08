\version "2.19.58"
\include "oll-core/package.ily"
\loadPackage lilypond-export

music = \new Staff {
  e'4( d')\prall <c' e' g'>8 ~ <c' d' g'>8 (\prall <c' f' a'>)
}

opts.exporter = #exportMusicXML
\score {
  \music
  \layout {
    \FileExport #opts
  }
}
