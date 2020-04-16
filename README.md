# mrtab
Stata module to compute one-way and two-way tables of multiple responses

`mrtab` tabulates multiple responses which are held as a set of indicator 
variables or as a set of polytomous response variables. Also see:

Jann, B. 2005. Tabulation of multiple responses. The Stata Journal 5(1): 92-122.
http://www.stata-journal.com/article.html?article=st0082

To install the `mrtab` package from the SSC Archive, type

    . ssc install mrtab, replace

in Stata. Stata version 8.2 or newer is required.

---

Installation from GitHub:

    . net install mrtab, replace from(https://raw.githubusercontent.com/benjann/mrtab/master/)

---

Main changes:

    16apr2020
    - in indicator mode, if the first variable had no label, no labels were
      displayed; this is fixed
    - in indicator mode, if a variable had no label, wrong results were displayed
      by mrgraph; this is fixed
    
    21jun2019
    - -warn- option added