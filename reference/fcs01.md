# Sample Food Consumption Score (FCS) data from World Food Programme (WFP) VAM Resource Centre

Sample Food Consumption Score (FCS) data from World Food Programme (WFP)
VAM Resource Centre

## Usage

``` r
fcs01
```

## Format

A data frame with 18 columns and 26 rows:

|                |                                                                        |
|----------------|------------------------------------------------------------------------|
| **Variable**   | **Description**                                                        |
| *FCSStap*      | Integer value from 0-7 for consumption frequency of staples            |
| *FCSStap_SRf*  | Staples food source                                                    |
| *FCSVeg*       | Integer value from 0-7 for consumption frequency of vegetables         |
| *FCSVeg_SRf*   | Vegetables food source                                                 |
| *FCSFruit*     | Integer value from 0-7 for consumption frequency of fruits             |
| *FCSFruit_SRf* | Fruits food source                                                     |
| *FCSPr*        | Integer value from 0-7 for consumption frequency of protein-rich foods |
| *FCSPr_SRf*    | Protein-rich food source                                               |
| *FCSPulse*     | Integer value from 0-7 for consumption frequency of pulses             |
| *FCSPulse_SRf* | Pulses food source                                                     |
| *FCSDairy*     | Integer value from 0-7 for consumption frequency of dairy              |
| *FCSDairy_SRf* | Dairy food source                                                      |
| *FCSSugar*     | Integer value from 0-7 for consumption frequency of sugary foods       |
| *FCSSugar_SRf* | Sugary food source                                                     |
| *FCSFat*       | Integer value from 0-7 for consumption frequency of fats               |
| *FCSFat_SRf*   | Fats food source                                                       |
| *FCSCond*      | Integer value from 0-7 for consumption frequency of condiments         |
| *FCSCond_SRf*  | Condiments food source                                                 |

## Source

https://github.com/WFP-VAM/RAMResourcesScripts/blob/main/Static/FCS_Sample_Survey.csv

## Examples

``` r
fcs01
#>    FCSStap FCSStap_SRf FCSVeg FCSVeg_SRf FCSFruit FCSFruit_SRf FCSPr FCSPr_SRf
#> 1        7         100      5        100        3          600     2       800
#> 2        1         200      2        200        3          700     1       999
#> 3        7         300      4        300        2          800     2      1000
#> 4        2         400      4        400        7          999     7       600
#> 5        1         500      1        500        3         1000     0       n/a
#> 6        2         600      2        800        3          300     5       200
#> 7        3         700      4        999        6          600     2       300
#> 8        6         800      1       1000        2          200     3       500
#> 9        5         999      1        300        2          300     0       n/a
#> 10       3        1000      1        100        2          100     1       400
#> 11       4         300      4        200        5          200     7       600
#> 12       0         n/a      7        300        7          300     5       700
#> 13       1         200      4        400        6          400     7       800
#> 14       0         n/a      5        500        3          500     0       n/a
#> 15       4         300      5        600        7          800     5       600
#> 16       0         n/a      4        700        6          999     1       700
#> 17       3         400      6        800        7         1000     7       800
#> 18       0         n/a      3        999        1          300     3       999
#> 19       0         n/a      3       1000        5          400     3      1000
#> 20       4         600      2        600        7          500     5       300
#> 21       3         700      6        200        3          600     5       600
#> 22       3         800      5        300        4          700     1       800
#> 23       1         999      3        400        1          800     3       999
#> 24       6        1000      3        500        1          999     5      1000
#> 25       0         n/a      0        n/a        0          n/a     0       n/a
#> 26       1         600      1        600        1         1000     1      1000
#>    FCSPulse FCSPulse_SRf FCSDairy FCSDairy_SRf FCSSugar FCSSugar_SRf FCSFat
#> 1         4          600        1          300        6          600      0
#> 2         2          700        5          600        4          700      2
#> 3         4          300        1          200        2          300      0
#> 4         5          600        0          n/a        5          600      0
#> 5         1          200        4          600        7          200      5
#> 6         1          300        4          300        4          300      7
#> 7         0          n/a        2          600        0          n/a      0
#> 8         4          400        2          700        0          n/a      4
#> 9         0          n/a        4          100        3          300      6
#> 10        6          500        5          200        4          600      4
#> 11        0          n/a        3          300        6          400      1
#> 12        0          n/a        5          400        7          100      5
#> 13        3          600        6          500        1          200      7
#> 14        3          700        4          800        4          300      5
#> 15        5          100        3          999        7          400      0
#> 16        5          200        6         1000        1          500      1
#> 17        6          300        3          400        6          600      0
#> 18        6          400        0          n/a        3          700      6
#> 19        3          500        0          n/a        5          800      7
#> 20        5          800        3          800        4          999      7
#> 21        6          999        3          999        6         1000      0
#> 22        6         1000        7         1000        6          300      5
#> 23        2          600        6          500        2          500      3
#> 24        2          600        1          600        6          800      7
#> 25        0          n/a        0          n/a        0          n/a      0
#> 26        1          700        1          700        1          700      1
#>    FCSFat_SRf FCSCond FCSCond_SRf
#> 1         n/a       2         800
#> 2         200       6         999
#> 3         n/a       7        1000
#> 4         n/a       0         n/a
#> 5         600       0         n/a
#> 6         300       0         n/a
#> 7         n/a       2         600
#> 8         400       0         n/a
#> 9         100       4         200
#> 10        200       0         n/a
#> 11        300       5         600
#> 12        400       1         300
#> 13        500       1         100
#> 14        500       4         200
#> 15        n/a       2         300
#> 16        600       5         400
#> 17        n/a       1         500
#> 18        300       2         600
#> 19       1000       1         700
#> 20       1000       7         300
#> 21        n/a       4         600
#> 22        300       0         n/a
#> 23        600       1         600
#> 24        800       5         400
#> 25        n/a       0         n/a
#> 26        700       1         500
```
