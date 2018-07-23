---
title: "R bookdownplus"
author: "Peng Zhao"
site: bookdown::bookdown_site
documentclass: article
output:
  bookdown::pdf_book: 
    includes:
      in_header: tex/template_yihui_mini.tex
    latex_engine: xelatex
    citation_package: natbib
    keep_tex: yes
biblio-style: apalike
link-citations: yes
colorlinks: no
lot: yes
lof: yes
geometry: [b5paper, tmargin=2.5cm, bmargin=2.5cm, lmargin=3.5cm, rmargin=2.5cm]
---

<!--chapter:end:index.Rmd-->

  In our increasingly interconnected world, it is crucial to
  understand the risk of an outbreak originating in one country/region
  and  spreading to the rest of the world. Rapid recognition and
  response to potential pandemics and emerging diseases have become
  essential global health priorities. Digital disease surveillance
  tools such as ProMed and HealthMap have the potential to serve as
  important early warning systems as well as complement the field
  surveillance data during an ongoing outbreak. While there are a
  number of systems that carry out digital disease surveillance, there
  is as yet a lack of tools that can compile and analyse the generated
  data to produce easily understood actionable reports. We present a 
  flexible statistical model that uses different streams
  of data (such as disease surveillance data, mobility data etc.) for
  short-term incidence trend forecasting.  
  In validating the model using data collected by ProMED and
  HealthMap during the 2014-2016 West African Ebola outbreak,
  we provide a realistic appraisal of the strengths and limitations
  of such data in incidence forecasting.
  We infer incidence trends at finer spatial scales from
  aggregated data. Our work shows how the data from
  event based surveillance systems (EBS) can complement the data
  collected from traditional public health infrastructure. During an
  ongoing crisis, combining data from different sources gives
  stakeholders a more complete picture.

<!--chapter:end:01-abstract.Rmd-->

# Introduction

other tools that do similar stuff - EpiDMS [@liu2016epidms]
slightly old paper - authors curated news themselves! 
[@chowell2016elucidating]

<!--chapter:end:02-introduction.Rmd-->

# Introduction

The R package `bookdownplus` [@R-bookdownplus] is an extension of `bookdown` [@R-bookdown]. It is a collection of
multiple templates on the basis of LaTeX, which are tailored so that I can work happily under the umbrella of `bookdown`. `bookdownplus` helps you write academic journal articles, guitar books, chemical equations, mails, calendars, and diaries.

# Features

`bookdownplus` extends the features of `bookdown`, and simplifies the procedure. Users only have to choose a template, clarify the book title and author name, and then focus on writing the text. No need to struggle in YAML and LaTeX. 

With `bookdownplus` users can

-   record guitar chords,

-   write a mail in an elegant layout,

-   write a laboratory journal, or a personal diary,

-   draw a monthly or weekly or conference calendar,

-   and, of course, write academic articles in your favourite way,

-   with chemical molecular formulae and equations,

-   even in Chinese,

-   and more wonders will come soon.

Full documentation can be found in the book [R bookdownplus Textbook](https://bookdown.org/baydap/bookdownplus). The webpage looks so-so, while the [pdf file](https://bookdown.org/baydap/bookdownplus/bookdownplus.pdf) might give you a little surprise.  

# Quick start

Although this section might not be the latest version, the general idea won't change. Please see [R bookdownplus Textbook](https://bookdown.org/baydap/bookdownplus) to keep up with the update.

## Preparation

Before starting, you have to install R, RStudio, bookdown package, and
other software and packages (i.e. Pandoc, LaTeX, rmarkdown, rticle,
knitr, etc.) which bookdown depends on. See the official [manual](https://bookdown.org/yihui/bookdown/) of
bookdown for details. Additionally, if you want to produce a poster, phython must be installed before using, and the path of phython might have to be added to the environmental variables for Windows users.

## Installation

```
install.package("bookdownplus")
# or
devtools::
  install_github("pzhaonet/bookdownplus")
```

## Generate demo files

Run the following codes, and you will get some files (e.g. `index.Rmd`, `body.Rmd`, `bookdownplus.Rproj`) and folders in your working directory.

```
getwd() # this is your working directory. run setwd() to change it.
bookdownplus::bookdownplus()
```

## Build a demo book

Now open `bookdownplus.Rproj` with RStudio, and press `ctrl+shift+b` to compile it. Your will get a book file named `*.pdf` in `_book/`folder.

## Write your own

Write your own text in `index.Rmd` and `body.Rmd`, and build your own lovely book.

## More outputs

By default, the book is in a pdf file. From 'bookdownplus' 1.0.3, users can get more output formats, including 'word', 'html' and 'epub'. Run:

```
bookdownplus::
  bookdownplus(template = 'article', 
               more_output = c('html', 'word', 'epub'))
```

## Recommendations

I have been developing some other packages, which bring more features into 'bookdown', such as:

- mindr [@R-mindr], which can extract the outline of your book and turn it into a mind map, and

- pinyin [@R-pinyin], which can automatically generate ['{#ID}'](https://bookdown.org/yihui/bookdown/cross-references.html) of the chapter headers even if there are Chinese characters in them.

Both of them have been released on CRAN and can be installed via:

```
install.packages('mindr')
install.packages('pinyin')
```

Enjoy your bookdowning!

## Models

Eq. \@ref(eq:mc2) is an equation.

\begin{equation} 
E = mc^2
  (\#eq:mc2)
\end{equation} 

It can be written as $E = mc^2$.


# Results

Fig. \@ref(fig:fig1) psum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 

\begin{figure}

{\centering \includegraphics[width=0.8\linewidth]{yihui_mini_files/figure-latex/fig1-1} 

}

\caption{caption}(\#fig:fig1)
\end{figure}

Tab. \@ref(tab:tab1) psum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 

\begin{table}

\caption{(\#tab:tab1)Here is a nice table!}
\centering
\begin{tabular}[t]{rrrrl}
\toprule
Sepal.Length & Sepal.Width & Petal.Length & Petal.Width & Species\\
\midrule
5.1 & 3.5 & 1.4 & 0.2 & setosa\\
4.9 & 3.0 & 1.4 & 0.2 & setosa\\
4.7 & 3.2 & 1.3 & 0.2 & setosa\\
4.6 & 3.1 & 1.5 & 0.2 & setosa\\
5.0 & 3.6 & 1.4 & 0.2 & setosa\\
\addlinespace
5.4 & 3.9 & 1.7 & 0.4 & setosa\\
4.6 & 3.4 & 1.4 & 0.3 & setosa\\
5.0 & 3.4 & 1.5 & 0.2 & setosa\\
4.4 & 2.9 & 1.4 & 0.2 & setosa\\
4.9 & 3.1 & 1.5 & 0.1 & setosa\\
\addlinespace
5.4 & 3.7 & 1.5 & 0.2 & setosa\\
4.8 & 3.4 & 1.6 & 0.2 & setosa\\
4.8 & 3.0 & 1.4 & 0.1 & setosa\\
4.3 & 3.0 & 1.1 & 0.1 & setosa\\
5.8 & 4.0 & 1.2 & 0.2 & setosa\\
\addlinespace
5.7 & 4.4 & 1.5 & 0.4 & setosa\\
5.4 & 3.9 & 1.3 & 0.4 & setosa\\
5.1 & 3.5 & 1.4 & 0.3 & setosa\\
5.7 & 3.8 & 1.7 & 0.3 & setosa\\
5.1 & 3.8 & 1.5 & 0.3 & setosa\\
\bottomrule
\end{tabular}
\end{table}

# Conclusions

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum

<!--chapter:end:body.Rmd-->

