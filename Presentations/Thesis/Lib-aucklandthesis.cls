% University of Auckland thesis class
%
% aucklandthesis.cls
%
% based on standard book.cls.
%
% This modification was adapted from uathesis.cls by Lee Middleton (2002).
%
% Simplified and updated for new regulations and broader applicability
% by Alistair Kwan (2015).
%
% Accommodates theses for degrees other than the PhD.
%
% This class does not intervene much — font and chapter heading customisation
% are left for the user to determine, and without locking users into or out of XeLaTeX.
%
%
% This file may be distributed and/or modified under the conditions of
% the LaTeX Project Public License, either version 1.2 of this license
% or (at your option) any later version.  The latest version of this
% license is in:
%
%    http://www.latex-project.org/lppl.txt
%
% and version 1.2 or later is part of all distributions of LaTeX version
% 1999/12/01 or later.
%


\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{aucklandthesis}
      [2016/01/01 University of Auckland thesis class]
\newif\ifUa@examcp
\newif\ifUa@singlesp
\newif\ifUa@onehalfsp
\newif\ifUa@doublesp
\newif\ifUa@partialfulfilment

\Ua@examcpfalse
\Ua@onehalfspfalse
\Ua@doublespfalse
\Ua@partialfulfilmentfalse

\DeclareOption{doublespace}{\Ua@doublesptrue}
\DeclareOption{singlespace}{\Ua@singlesptrue}
\DeclareOption{onehalfspace}{\Ua@onehalfsptrue}
\DeclareOption{examcopy}{\Ua@examcptrue}
\DeclareOption{partial}{\Ua@partialfulfilmenttrue}
\DeclareOption{twoside}{\PassOptionsToPackage{twoside}{geometry}}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{book}}

\ExecuteOptions{oneside}
\ProcessOptions\relax

\LoadClass{book}

\pdfpagewidth=\paperwidth
\pdfpageheight=\paperheight
\RequirePackage[
     pdftex,
     a4paper,
     bindingoffset=4mm,
     left=30mm,
     right=30mm,
     top=30mm,
     bottom=30mm,
     nomarginpar
  ]{geometry}

\RequirePackage{setspace}
\RequirePackage{fancyhdr}
\RequirePackage{textcase}
\raggedbottom

\pagestyle{fancyplain}
\addtolength{\headheight}{2.5pt}
\addtolength{\headwidth}{0mm}
\renewcommand{\chaptermark}[1]{\markboth{#1}{}}
\renewcommand{\sectionmark}[1]{\markright{\thesection\ #1}}
\fancyhf{} % no headers or footers
\fancyhead[LE,RO]{\thepage}
\fancyhead[LO]{\rightmark}
\fancyhead[RE]{\leftmark}
\fancypagestyle{plain}{%
  \fancyhf{}
  \fancyfoot[C]{\thepage}
  \renewcommand{\headrulewidth}{0pt}
  \renewcommand{\footrulewidth}{0pt}
}

\AtBeginDocument{%
}

\def\@title{}
\def\@author{}
\def\degreesought#1{\gdef\@degreesought{#1}}
\def\@degreesought{\@latex@error{No \noexpand\degreesought given}\@ehc}
\def\degreediscipline#1{\gdef\@degreediscipline{#1}}
\def\@degreediscipline{\@latex@error{No \noexpand\degreediscipline given}\@ehc}
\def\degreecompletionyear#1{\gdef\@degreecompletionyear{#1}}
\def\@degreecompletionyear{\@latex@error{No \noexpand\degreecompletionyear given}\@ehc}

\renewcommand{\maketitle}{
	\cleardoublepage
	\thispagestyle{empty}
	\newgeometry{top=5mm,bottom=5mm}
	\setcounter{page}{1}
	\noindent\begin{minipage}[t][0.3\textheight][t]{\textwidth}
	\vspace*{\fill}
	\begin{center}
		\huge \@title \par
	\end{center}
	\vspace*{\fill}
	\end{minipage}

	\noindent\begin{minipage}[t][0.3\textheight][t]{\textwidth}
	\vspace*{\fill}
	\begin{center}
		\large
		\@author
	\end{center}
	\vspace*{\fill}
	\end{minipage}

	\noindent\begin{minipage}[t][0.3\textheight][t]{\textwidth}
	\vspace*{\fill}
	\begin{center}
		\normalfont
		A thesis submitted in
		\ifUa@partialfulfilment
			partial
		\fi
		fulfilment of the requirements for the degree of

		\@degreesought\
		in
		\@degreediscipline,

		The University of Auckland,
		\@degreecompletionyear.

		\ifUa@examcp
			\bigskip
			This thesis is for examination purposes only,

			and is confidential to the examination process.
		\fi
	\end{center}
	\vspace*{\fill}
	\end{minipage}

	\clearpage
	\restoregeometry
	\mainmatter
}

\newcommand\thesisdedication[1]{
	\cleardoublepage
\thispagestyle{plain}
\vspace*{\fill}
\begin{center}
#1
\end{center}
\vspace*{\fill}
\vspace*{\fill}
}

\renewcommand\mainmatter{
	\ifUa@doublesp
   \doublespacing%
\else\ifUa@singlesp
   \singlespacing%
\else
   \onehalfspacing
\fi%
\fi
}

\renewcommand\backmatter{
	\clearpage
	\setcounter{page}{0}
}

% \chapter{} adjusted for uppercase TOC entry
\renewcommand\chapter{\if@openright\cleardoublepage\else\clearpage\fi
                    \thispagestyle{plain}%
                    \global\@topnum\z@
                    \@afterindentfalse
                    \secdef\@chapter\@schapter}
\def\@chapter[#1]#2{\ifnum \c@secnumdepth >\m@ne
                       \if@mainmatter
                         \refstepcounter{chapter}%
                         \typeout{\@chapapp\space\thechapter.}%
                         \addcontentsline{toc}{chapter}%
                                   {\protect\numberline{\thechapter}#1}%
                       \else
                         \addcontentsline{toc}{chapter}{#1}%
                       \fi
                    \else
                      \addcontentsline{toc}{chapter}{#1}%
                    \fi
                    \chaptermark{#1}%
                    \addtocontents{lof}{\protect\addvspace{10\p@}}%
                    \addtocontents{lot}{\protect\addvspace{10\p@}}%
                    \if@twocolumn
                      \@topnewpage[\@makechapterhead{#2}]%
                    \else
                      \@makechapterhead{#2}%
                      \@afterheading
                    \fi}

\endinput
