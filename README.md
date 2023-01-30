
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ranchr

<!-- badges: start -->
<!-- badges: end -->

`ranchr` provides data and functions to help you breed great monsters in
the game [Monster Rancher
2](https://en.wikipedia.org/wiki/Monster_Rancher_2).

## Installation

You can install the development version of `ranchr` like so:

``` r
# install.packages("devtools")
devtools::install_github("duckmayr/ranchr")
```

## Example

Suppose you had a Mono Eyed (Tiger/Suezo) and Mustardy (Zuum/Suezo) and
wanted to know what their combination prospects were. First you’d create
`Monster` class objects representing these parent monsters (their
current stats are given in the `stats` argument):

``` r
library(ranchr)
Parent1 = Monster(
    name  = "Collie",
    main  = "Tiger",
    sub   = "Suezo",
    stats = c(150, 111, 612, 680, 504, 95)
)

Parent2 = Monster(
    name  = "Marigold",
    main  = "Zuum",
    sub   = "Suezo",
    stats = c(286, 220, 461, 505, 400, 181)
)
```

To get a great monster combination, you need the two parents’ *adjusted*
stats to match with each other, as well as matching with the child
monster type’s *baseline* stats. You know how when you’re training a
monster, some stats go up faster than others? Well the same monster
attributes that cause these differences in stat gains also lead to a
behind-the-scenes “adjustment” in how the stat levels are treated during
combination. “Baseline” stats just refers to the average starting out
attributes for a monster type straight from a disc. If you examine a
`Monster` object, it will give you all this information:

``` r
Parent1
#> 
#>                                      Collie                                     
#> 
#>                                    Mono Eyed                                    
#>                                 (Tiger / Suezo)                                 
#> 
#>                Stat       Current      Gain      Adjusted   Baseline            
#>            ---------------------------------------------------------            
#>            Life               150   2 (x 0.50)         75         90            
#>            Power              111   2 (x 0.50)         56         80            
#>            Intelligence       612   4 (x 1.50)        918        130            
#>            Skill              680   5 (x 2.00)        999        170            
#>            Speed              504   3 (x 1.00)        504        100            
#>            Defense             95   1 (x 0.00)          0         60            
#> 
#>                 Stat order:                                                     
#>                 Skill, Intelligence, Speed, Life, Power, Defense
```

To test out and see how to `Monster`s would combine, you simply add them
together!

``` r
Parent1 + Parent2
#> 
#>     Combining Mono Eyed with Mustardy
#> 
#>                        Parents' Adjusted & Ordered Stats                        
#>                            Mono Eyed        Mustardy                            
#>                          Tiger / Suezo    Zuum / Suezo                          
#>                         -------------------------------                         
#>                             Sk: 999         Sk: 758                             
#>                             I:  918         I:  461                             
#>                             Sp: 504         Sp: 400                             
#>                             L:   75         L:  286                             
#>                             P:   56         P:  220                             
#>                             D:    0         D:  181                             
#> 
#>     Dadge says: The prospect of this combination is great. It can't go wrong
#>                 unless something weird happens
#> 
#>     (That means there are 6 matching stats)
#> 
#>                                Possible Outcomes                                
#>                Monster      Main     Sub     Stat Order     Matches             
#>             --------------------------------------------------------            
#>              Mono Eyed      Tiger   Suezo   Sk I Sp L P D      6                
#>              Tiger          Tiger   Tiger   Sk Sp I P L D      2                
#>              Datonare       Tiger   Zuum    Sk Sp I D P L      2                
#>              Mustardy       Zuum    Suezo   Sk L D Sp P I      2                
#>              HoundSaurian   Zuum    Tiger   Sk L P Sp I D      2                
#>              Zuum           Zuum    Zuum    Sk L P D Sp I      1                
#>              Suezo          Suezo   Suezo   I Sk P D Sp L      0                
#>              Horn           Suezo   Tiger   I Sk P Sp D L      0                
#>              Melon Suezo    Suezo   Zuum    I Sk P D Sp L      0
```

You can see that this should be a great combination, particularly if you
can get another Mono Eyed (Tiger/Suezo) out of it, since both parents
and the child monster would have their stats ordered: Skill,
Intelligence, Speed, Life, Power, Defense (abbreviated Sk, I, Sp, L, P,
D respectively in the output above).

## Special Thanks

Special thanks to Kurasu Soratobu, whose “Monster Rancher: Combining
FAQ” provided the necessary data and information to make this package
possible; I appreciate his willingness to let me publish his data.
Please see the file `inst/COPYRIGHTS` for relevant copyright
information.

## Discrepancies

The data for Kurasu’s guide were compiled several years ago for the
PlayStation version of Monster Rancher 2. It’s *possible* that some
things have changed when they ported the game to iOS, Nintendo Switch,
and PC. If you notice anything that seems off, just [open an
issue](https://github.com/duckmayr/ranchr/issues) and let me know.
