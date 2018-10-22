# LilyPond Export

In this repository I am creating an export-infrastructure for LilyPond.
The files presented here are far from production-ready, but at least the functions provided here can
create humdrum and musicXML for very simple scores.

With the *(pre-alpha-stage)* OLL-module this file

```lilypond
\version "2.19.82"
\include "oll-core/package.ily"
\loadPackage lilypond-export

music = \new PianoStaff <<
  \new Staff <<
    { \time 3/4 \key es \major \set Timing.tempoWholesPerMinute = #(ly:make-moment 30) \partial 4 s4 | \repeat volta 2 { s2.*3 } }
    \relative <<
      { b'4 | c4. a8 g4 | g( bes) <g b> | \tuplet 3/2 { a c a~ } a | } \\
      { r4 | e8 f g fis e4 | es2 d4 | <c e>8[ <b dis> <bes d>] <a cis>  <c f>4 }
    >>
  >>
  \new Staff {
    \time 3/4 \clef bass \key es \major
    \new Voice = "mel" \relative { g4 | c2 c4 | c g b | a2. | }
  }
  \new Lyrics \lyricsto "mel" { \lyricmode { la la le li lu la lo } }
  \new Lyrics \lyricsto "mel" { \lyricmode { ku ka ke ki ku ka ko } }
>>

% exporter can run without actually typesetting
\exportMusic \default hum \music

opts.exporter = #exportMusicXML
% or as a layout extension that is added to the layout
\score {
  \music
  \layout {
    \FileExport #opts
  }
  \midi {}
}
```

creates a humdrum file (.krn):

```humdrum
**kern	**kern
*staff2	*staff1
*	*^
=0	=0	=0
*clefF4	*clefG2	*clefG2
*M3/4	*M3/4	*M3/4
*k[e-a-b-]	*k[e-a-b-]	*k[e-a-b-]
4G	4b	4r
=1	=1	=1
2c	4.cc	8eL
.	.	8f
.	.	8g
.	8a	8f#J
4c	4g	4e
=2	=2	=2
4c	4g	2e-
4G	4b-	.
4B	4g b	4d
=3	=3	=3
2.A	6a	8c eL
.	.	8B d#
.	6cc	.
.	.	8B- dJ
.	6a	.
.	.	8A c#
.	4a	4c f
=4	=4	=4
=4	=4	=4
*-	*-	*-

```

and an XML-file:

```xml
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE score-partwise
  PUBLIC '-//Recordare//DTD MusicXML 3.0 Partwise//EN'
  'http://www.musicxml.org/dtds/partwise.dtd'>
<score-partwise version="3.0">
	<part-list>
		<score-part id="P1">
			<part-name>Part 1</part-name>
		</score-part>
		<score-part id="P2">
			<part-name>Part 2</part-name>
		</score-part>
	</part-list>
	<part id="P1">
		<measure number="0">
			<attributes>
				<divisions>128</divisions>
				<time>
					<beats>3</beats>
					<beat-type>4</beat-type>
				</time>
			</attributes>
			<note>
				<pitch>
					<step>B</step>
					<octave>4</octave>
				</pitch>
				<duration>128</duration>
				<voice>1</voice>
				<type>quarter</type>
			</note>
			<backup>
				<duration>128</duration>
			</backup>
			<note>
				<rest/>
				<duration>128</duration>
				<voice>2</voice>
				<type>quarter</type>
			</note>
		</measure>
		<measure number="1">
			<attributes>
				<divisions>128</divisions>
			</attributes>
			<note>
				<pitch>
					<step>C</step>
					<octave>5</octave>
				</pitch>
				<duration>192</duration>
				<voice>1</voice>
				<type>quarter</type>
				<dot/>
			</note>
			<note>
				<pitch>
					<step>A</step>
					<octave>4</octave>
				</pitch>
				<duration>64</duration>
				<voice>1</voice>
				<type>eighth</type>
				<accidental>natural</accidental>
			</note>
			<note>
				<pitch>
					<step>G</step>
					<octave>4</octave>
				</pitch>
				<duration>128</duration>
				<voice>1</voice>
				<type>quarter</type>
			</note>
			<backup>
				<duration>384</duration>
			</backup>
			<note>
				<pitch>
					<step>E</step>
					<octave>4</octave>
				</pitch>
				<duration>64</duration>
				<voice>2</voice>
				<type>eighth</type>
				<accidental>natural</accidental>
				<beam number="1">begin</beam>
			</note>
			<note>
				<pitch>
					<step>F</step>
					<octave>4</octave>
				</pitch>
				<duration>64</duration>
				<voice>2</voice>
				<type>eighth</type>
				<beam number="1">continue</beam>
			</note>
			<note>
				<pitch>
					<step>G</step>
					<octave>4</octave>
				</pitch>
				<duration>64</duration>
				<voice>2</voice>
				<type>eighth</type>
				<beam number="1">continue</beam>
			</note>
			<note>
				<pitch>
					<step>F</step>
					<alter>1</alter>
					<octave>4</octave>
				</pitch>
				<duration>64</duration>
				<voice>2</voice>
				<type>eighth</type>
				<accidental>sharp</accidental>
				<beam number="1">end</beam>
			</note>
			<note>
				<pitch>
					<step>E</step>
					<octave>4</octave>
				</pitch>
				<duration>128</duration>
				<voice>2</voice>
				<type>quarter</type>
			</note>
		</measure>
		<measure number="2">
			<attributes>
				<divisions>128</divisions>
			</attributes>
			<note>
				<pitch>
					<step>G</step>
					<octave>4</octave>
				</pitch>
				<duration>128</duration>
				<voice>1</voice>
				<type>quarter</type>
			</note>
			<note>
				<pitch>
					<step>B</step>
					<alter>-1</alter>
					<octave>4</octave>
				</pitch>
				<duration>128</duration>
				<voice>1</voice>
				<type>quarter</type>
			</note>
			<note>
				<pitch>
					<step>G</step>
					<octave>4</octave>
				</pitch>
				<duration>128</duration>
				<voice>1</voice>
				<type>quarter</type>
			</note>
			<note>
				<chord/>
				<pitch>
					<step>B</step>
					<octave>4</octave>
				</pitch>
				<duration>128</duration>
				<voice>1</voice>
				<type>quarter</type>
				<accidental>natural</accidental>
			</note>
			<backup>
				<duration>384</duration>
			</backup>
			<note>
				<pitch>
					<step>E</step>
					<alter>-1</alter>
					<octave>4</octave>
				</pitch>
				<duration>256</duration>
				<voice>2</voice>
				<type>half</type>
			</note>
			<note>
				<pitch>
					<step>D</step>
					<octave>4</octave>
				</pitch>
				<duration>128</duration>
				<voice>2</voice>
				<type>quarter</type>
			</note>
		</measure>
		<measure number="3">
			<attributes>
				<divisions>128</divisions>
			</attributes>
			<note>
				<pitch>
					<step>A</step>
					<octave>4</octave>
				</pitch>
				<duration>85</duration>
				<voice>1</voice>
				<type>quarter</type>
				<accidental>natural</accidental>
				<time-modification>
					<actual-notes>3</actual-notes>
					<normal-notes>2</normal-notes>
				</time-modification>
				<notations>
					<tuplet number="1" placement="above" type="start"/>
				</notations>
			</note>
			<note>
				<pitch>
					<step>C</step>
					<octave>5</octave>
				</pitch>
				<duration>85</duration>
				<voice>1</voice>
				<type>quarter</type>
				<time-modification>
					<actual-notes>3</actual-notes>
					<normal-notes>2</normal-notes>
				</time-modification>
			</note>
			<note>
				<pitch>
					<step>A</step>
					<octave>4</octave>
				</pitch>
				<duration>86</duration>
				<voice>1</voice>
				<type>quarter</type>
				<time-modification>
					<actual-notes>3</actual-notes>
					<normal-notes>2</normal-notes>
				</time-modification>
				<notations>
					<tuplet number="1" placement="above" type="stop"/>
				</notations>
			</note>
			<note>
				<pitch>
					<step>A</step>
					<octave>4</octave>
				</pitch>
				<duration>128</duration>
				<voice>1</voice>
				<type>quarter</type>
			</note>
			<backup>
				<duration>384</duration>
			</backup>
			<note>
				<pitch>
					<step>C</step>
					<octave>4</octave>
				</pitch>
				<duration>64</duration>
				<voice>2</voice>
				<type>eighth</type>
				<beam number="1">begin</beam>
			</note>
			<note>
				<chord/>
				<pitch>
					<step>E</step>
					<octave>4</octave>
				</pitch>
				<duration>64</duration>
				<voice>2</voice>
				<type>eighth</type>
				<accidental>natural</accidental>
				<beam number="1">begin</beam>
			</note>
			<note>
				<pitch>
					<step>B</step>
					<octave>3</octave>
				</pitch>
				<duration>64</duration>
				<voice>2</voice>
				<type>eighth</type>
				<accidental>natural</accidental>
				<beam number="1">continue</beam>
			</note>
			<note>
				<chord/>
				<pitch>
					<step>D</step>
					<alter>1</alter>
					<octave>4</octave>
				</pitch>
				<duration>64</duration>
				<voice>2</voice>
				<type>eighth</type>
				<accidental>sharp</accidental>
				<beam number="1">continue</beam>
			</note>
			<note>
				<pitch>
					<step>B</step>
					<alter>-1</alter>
					<octave>3</octave>
				</pitch>
				<duration>64</duration>
				<voice>2</voice>
				<type>eighth</type>
				<accidental>flat</accidental>
				<beam number="1">end</beam>
			</note>
			<note>
				<chord/>
				<pitch>
					<step>D</step>
					<octave>4</octave>
				</pitch>
				<duration>64</duration>
				<voice>2</voice>
				<type>eighth</type>
				<accidental>natural</accidental>
				<beam number="1">end</beam>
			</note>
			<note>
				<pitch>
					<step>A</step>
					<octave>3</octave>
				</pitch>
				<duration>64</duration>
				<voice>2</voice>
				<type>eighth</type>
				<accidental>natural</accidental>
			</note>
			<note>
				<chord/>
				<pitch>
					<step>C</step>
					<alter>1</alter>
					<octave>4</octave>
				</pitch>
				<duration>64</duration>
				<voice>2</voice>
				<type>eighth</type>
				<accidental>sharp</accidental>
			</note>
			<note>
				<pitch>
					<step>C</step>
					<octave>4</octave>
				</pitch>
				<duration>128</duration>
				<voice>2</voice>
				<type>quarter</type>
				<accidental>natural</accidental>
			</note>
			<note>
				<chord/>
				<pitch>
					<step>F</step>
					<octave>4</octave>
				</pitch>
				<duration>128</duration>
				<voice>2</voice>
				<type>quarter</type>
			</note>
		</measure>
		<measure number="4">
			<attributes>
				<divisions>128</divisions>
			</attributes>
		</measure>
	</part>
	<part id="P2">
		<measure number="0">
			<attributes>
				<divisions>128</divisions>
				<time>
					<beats>3</beats>
					<beat-type>4</beat-type>
				</time>
				<clef>
					<sign>F</sign>
					<line>4</line>
				</clef>
			</attributes>
			<attributes>
				<clef>
					<sign>F</sign>
					<line>4</line>
				</clef>
			</attributes>
			<note>
				<pitch>
					<step>G</step>
					<octave>3</octave>
				</pitch>
				<duration>128</duration>
				<voice>2</voice>
				<type>quarter</type>
				<lyric>
					<syllabic>single</syllabic>
					<text>la</text>
				</lyric>
				<lyric>
					<syllabic>single</syllabic>
					<text>ku</text>
				</lyric>
			</note>
		</measure>
		<measure number="1">
			<attributes>
				<divisions>128</divisions>
			</attributes>
			<note>
				<pitch>
					<step>C</step>
					<octave>4</octave>
				</pitch>
				<duration>256</duration>
				<voice>2</voice>
				<type>half</type>
				<lyric>
					<syllabic>single</syllabic>
					<text>la</text>
				</lyric>
				<lyric>
					<syllabic>single</syllabic>
					<text>ka</text>
				</lyric>
			</note>
			<note>
				<pitch>
					<step>C</step>
					<octave>4</octave>
				</pitch>
				<duration>128</duration>
				<voice>2</voice>
				<type>quarter</type>
				<lyric>
					<syllabic>single</syllabic>
					<text>le</text>
				</lyric>
				<lyric>
					<syllabic>single</syllabic>
					<text>ke</text>
				</lyric>
			</note>
		</measure>
		<measure number="2">
			<attributes>
				<divisions>128</divisions>
			</attributes>
			<note>
				<pitch>
					<step>C</step>
					<octave>4</octave>
				</pitch>
				<duration>128</duration>
				<voice>2</voice>
				<type>quarter</type>
				<lyric>
					<syllabic>single</syllabic>
					<text>li</text>
				</lyric>
				<lyric>
					<syllabic>single</syllabic>
					<text>ki</text>
				</lyric>
			</note>
			<note>
				<pitch>
					<step>G</step>
					<octave>3</octave>
				</pitch>
				<duration>128</duration>
				<voice>2</voice>
				<type>quarter</type>
				<lyric>
					<syllabic>single</syllabic>
					<text>lu</text>
				</lyric>
				<lyric>
					<syllabic>single</syllabic>
					<text>ku</text>
				</lyric>
			</note>
			<note>
				<pitch>
					<step>B</step>
					<octave>3</octave>
				</pitch>
				<duration>128</duration>
				<voice>2</voice>
				<type>quarter</type>
				<accidental>natural</accidental>
				<lyric>
					<syllabic>single</syllabic>
					<text>la</text>
				</lyric>
				<lyric>
					<syllabic>single</syllabic>
					<text>ka</text>
				</lyric>
			</note>
		</measure>
		<measure number="3">
			<attributes>
				<divisions>128</divisions>
			</attributes>
			<note>
				<pitch>
					<step>A</step>
					<octave>3</octave>
				</pitch>
				<duration>384</duration>
				<voice>2</voice>
				<type>half</type>
				<dot/>
				<accidental>natural</accidental>
				<lyric>
					<syllabic>single</syllabic>
					<text>lo</text>
				</lyric>
				<lyric>
					<syllabic>single</syllabic>
					<text>ko</text>
				</lyric>
			</note>
		</measure>
		<measure number="4">
			<attributes>
				<divisions>128</divisions>
			</attributes>
		</measure>
	</part>
</score-partwise>
```


