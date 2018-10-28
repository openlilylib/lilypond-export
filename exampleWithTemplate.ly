\version "2.19.82"
\include "oll-core/package.ily"
\loadPackage lalily-templates
\loadPackage lilypond-export

\optionsInit opts
\setDefaultTemplate brahms.chor.lindern lalily.vocal.group #opts
\setTitle "Töne, lindernder Klang"
\setSubTitle "Kanon für Sopran, Alt, Tenor und Baß"
\setHeader #'composer "Johannes Brahms"
\setSubSubTitle "(Veröffentlicht 1872)"
%\setHeader #'copyright-year "2012"

expopts.exporter = #exportMusicXML
\layout {
  \FileExport #expopts
}

\putMusic meta {
  \set Score.proportionalNotationDuration = #(ly:make-moment 1/8)
  \tempo "Andante"
  \time 6/8 \key e \minor
  s2.*3 \repeat volta 2 { s2.*10 }
  \alternative {
    { \set Score.repeatCommands = #`((volta ,#{ \markup { \text \fontsize #2 { Zur Wiederholung } } #})) s2.*2 }
    { \set Score.repeatCommands = #`((volta ,#{ \markup { \text \fontsize #2 { Zum Schluß } } #}) end-repeat) s2.*2 }
  }
  \bar "|."
}

\putMusic sop.music \relative c' {
  e4 fis8 g e g | b4. b | ais a | gis4( bis8) cis4 cis8 | dis4. gis,4 r8 |
  r4 r8 ees'4( bes8) | a4 c8 des4.~ | des8 c8 bes aes4 r8 | r4 r8 c4.~ | c8 bes a! g bes! g | d4 r8 r4 r8 |
  a'4. dis,4 r8 | e4 fis!8 g e g | b4. b | ais a |
  b2. ~ | b\fermata
}
\putMusic sop.lyrics \lyricmode {
  Tö -- ne, lin -- dern -- der Klang, du kannst nicht neh -- men die Schmer -- zen
  a -- ber die Tö -- ne viel -- leicht lin -- dern die lei -- den -- de Brust.
  Tö -- ne, tö -- ne, lin -- dern -- der Klang, du kannst nicht
  Klang! __
}

\putMusic alt.music \relative c' {
  R2. | b4 cis8 d b d | fis4. fis | eis e | dis4( fisis8) gis4 gis8 |
  bes4. ees,4 r8 | r4 r8 bes'4( f!8) | e!4 g8 aes4. ~ | aes8 g f ees4 r8 | r4 r8 g4. ~ | g8 f! e! d f d |
  a4 r8 r4 r8 | e'4. ais, | b4 cis8 d b d | fis4. fis |
  b,4 cis8 dis b dis | e2.\fermata |
}
\putMusic alt.lyrics \lyricmode {
  Tö -- ne, lin -- dern -- der Klang, du kannst nicht neh -- men die Schmer -- zen
  a -- ber die Tö -- ne viel -- leicht lin -- dern die lei -- den -- de Brust.
  Tö -- ne, tö -- ne, lin -- dern -- der Klang, du
  tö -- ne, lin -- dern -- der Klang!
}

\putMusic ten.music \relative c {
  R2.*2 | fis4 gis8 a fis a | cis4. cis | bis b |
  bes4( d8) ees4 ees8 | f!4. bes,4 r8 | r4 r8 f'!4( c8) | b!4 d8 ees4. ~ | ees8 d c bes4 r8 | r4 r8 d4. ~ |
  d8 c b! a c a | e4 r8 r4 r8 | b'4. eis,4 r8 | fis4 gis8 a fis a |
  b4.( a) | gis2.\fermata |
}
\putMusic ten.lyrics \lyricmode {
  Tö -- ne, lin -- dern -- der Klang, du kannst nicht neh -- men die Schmer -- zen
  a -- ber die Tö -- ne viel -- leicht lin -- dern die lei -- den -- de Brust.
  Tö -- ne, tö -- ne, lin -- dern -- der
  Tö -- ne!
}

\putMusic bas.music \relative c {
  R2.*3 | cis4 dis8 e cis e | gis4. gis |
  g! ges | f4( a8) bes4 bes8 | c!4. f,!4 r8 | r4 r8 c'4( g8) | fis4 a8 bes4. ~ | bes8 a g f4 r8 |
  r4 r8 a4. ~ | a8 g8 fis! e g e | b4 r8 r4 r8 | fis'4. bis, |
  b4 r8 b4. | e,2.\fermata |
}
\putMusic bas.lyrics \lyricmode {
  Tö -- ne, lin -- dern -- der Klang, du kannst nicht neh -- men die Schmer -- zen
  a -- ber die Tö -- ne viel -- leicht lin -- dern die lei -- den -- de Brust.
  Tö -- ne,
  Brust, die Brust.
}

\lalilyTest



%{
convert-ly (GNU LilyPond) 2.19.36  convert-ly: Processing `'...
Applying conversion: 2.19.2, 2.19.7, 2.19.11, 2.19.16, 2.19.22,
2.19.24, 2.19.28, 2.19.29, 2.19.32
%}
