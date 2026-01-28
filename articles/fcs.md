# Food Consumption Score

The **Food Consumption Score (FCS)** is an index developed by the World
Food Programme (WFP) in 1996. The **FCS** is a household level indicator
that aggregates food group diversity and frequency over the past 7 days.
These food groups are then weighted according to their relative
nutritional value. This means that food groups that are
nutritionally-dense such as animal products are given greater weight
than those containing less nutritionally dense foods such as tubers. The
weights are then added up to come up with a household score which are
then used to classify households into either poor, borderline, or
acceptable food consumption. The **FCS** is a measure of quantity of
caloric intake.

## FCS questionnaire

A brief questionnaire is used to ask respondents about the frequency of
their households’ consumption of eight different food groups over the
previous seven days. The eight food groups are:

1.  Main staples
2.  Pulses
3.  Vegetables
4.  Fruit
5.  Meat/fish
6.  Milk
7.  Sugar
8.  Oil

Following is a model questionnaire that can be used for collecting data
on **FCS**.

**Question:** I would like to ask you about all the different foods that
your household members have eaten in the last 7 days. Could you please
tell me how many days in the past week your household has eaten the
following foods?

*(for each food, ask what the primary source of each food item eaten
that week was, as well as the second main source of food, if any)*

|     | Food item                           | Number of days eaten in the past | Primary source+ | Secondary source+ |
|:---:|-------------------------------------|----------------------------------|-----------------|-------------------|
|  1  | Maize                               |                                  |                 |                   |
|  2  | Rice                                |                                  |                 |                   |
|  3  | Bread/wheat                         |                                  |                 |                   |
|  4  | Tubers                              |                                  |                 |                   |
|  5  | Groundnuts and pulses               |                                  |                 |                   |
|  6  | Fish (eaten as a main food)         |                                  |                 |                   |
|  7  | Fish powder (used for flavour only) |                                  |                 |                   |
|  8  | Red meat (sheep/goat/beef)          |                                  |                 |                   |
|  9  | White meat (poultry)                |                                  |                 |                   |
| 10  | Vegetable oil, fats                 |                                  |                 |                   |
| 11  | Eggs                                |                                  |                 |                   |
| 12  | Milk and dairy products (main food) |                                  |                 |                   |
| 13  | Milk in tea in small amounts        |                                  |                 |                   |
| 14  | Vegetables (including leaves)       |                                  |                 |                   |
| 15  | Fruits                              |                                  |                 |                   |
| 16  | Sweets, sugar                       |                                  |                 |                   |

| **+Food source codes:**                                                                                                                             |     |
|-----------------------------------------------------------------------------------------------------------------------------------------------------|-----|
| Purchase = 1; Own production = 2; Traded goods/services, barter = 3; \| Borrowed = 4; Received as gifts = 5; Food aid = 6; Others: (specify) = 7 \| |     |

This model questionnaire should be adapted to each survey context in
which it is to be used.

## Calculating the FCS

The **FCS** or the frequency weighted diet diversity score is a score
calculated using the frequency of consumption of different food groups
consumed by a household during the 7 days before the survey. Following
are the steps to calculate the FCS:

**Step 1.** Using the standard questionnaire above, group all the food
items into specific food groups (see groups in table below).

|     | Food items                                                                                                                                 | Food groups   | Weight |
|:---:|--------------------------------------------------------------------------------------------------------------------------------------------|---------------|--------|
|  1  | Maize , maize porridge, rice, sorghum, millet pasta, bread and other cereals Cassava, potatoes and sweet potatoes, other tubers, plantains | Main staples  | 2      |
|  2  | Beans, peas, groundnuts and cashew nuts                                                                                                    | Pulses        | 3      |
|  3  | Vegetables, leaves                                                                                                                         | Vegetables    | 1      |
|  4  | Fruits                                                                                                                                     | Fruits        | 1      |
|  5  | Beef, goat, poultry, pork, eggs and fish                                                                                                   | Meat and fish | 4      |
|  6  | Milk yoghurt and other diary                                                                                                               | Milk          | 4      |
|  7  | Sugar and sugar products, honey                                                                                                            | Sugar         | 0.5    |
|  8  | Oils, fats and butter                                                                                                                      | Oil           | 0.5    |
|  9  | Spices, tea, coffee, salt, fish powder, small amounts of milk for tea.                                                                     | Condiments    | 0      |

**Step 2.** Sum all the consumption frequencies of food items of the
same group, and recode the value of each group above 7 as 7.

**Step 3.** Multiply the value obtained for each food group by its
weight (see food group weights in table below) and create new weighted
food group scores.

**Step 4.** Sum the weighed food group scores, thus creating the food
consumption score (FCS).

**Step 5.** Using the appropriate thresholds (see below), recode the
variable food consumption score, from a continuous variable to a
categorical variable.

| **FCS**   | **Profiles** |
|-----------|--------------|
| 0 - 21    | Poor         |
| 21.5 - 35 | Borderline   |
| \> 35     | Acceptable   |

## Calculating the FCS using `dietry`

Steps 1, 2, 3, and 4 described above are implemented by the `dietry`
package using the
[`fcs_recode()`](https://nutriverse.io/dietry/reference/fcs_recode.md)
and the
[`fcs_calculate()`](https://nutriverse.io/dietry/reference/fcs_calculate.md)
functions. The
[`fcs_recode()`](https://nutriverse.io/dietry/reference/fcs_recode.md)
function is called within
[`fcs_calculate()`](https://nutriverse.io/dietry/reference/fcs_calculate.md)
to perform Step 1 and 2 and then
[`fcs_calculate()`](https://nutriverse.io/dietry/reference/fcs_calculate.md)
completes the operation by implementing steps 3 and 4. Using the `fcs01`
dataset, we apply the
[`fcs_calculate()`](https://nutriverse.io/dietry/reference/fcs_calculate.md)
function to implement these first 3 steps:

``` r
## Create a variable map to match food group labels to variables in fcs01 ----
var_map <- fcs_fg_map_variables(
  staples = "FCSStap",
  pulses = "FCSPulse",
  vegetables = "FCSVeg",
  fruits = "FCSFruit",
  meatfish = "FCSPr",
  milk = "FCSDairy",
  sugar = "FCSSugar",
  oil = "FCSFat",
  condiment = "FCSCond"
) 

## Calculate scores ----
fcs_calculate(df = fcs01, var_map = var_map, add = TRUE)
```

This provides the following output:

    #>    FCSStap FCSPulse FCSVeg FCSFruit FCSPr FCSDairy FCSSugar FCSFat FCSCond  fcs
    #> 1        7        4      5        3     2        1        6      0       2 49.0
    #> 2        1        2      2        3     1        5        4      2       6 40.0
    #> 3        7        4      4        2     2        1        2      0       7 45.0
    #> 4        2        5      4        7     7        0        5      0       0 60.5
    #> 5        1        1      1        3     0        4        7      5       0 31.0
    #> 6        2        1      2        3     5        4        4      7       0 53.5
    #> 7        3        0      4        6     2        2        0      0       2 32.0
    #> 8        6        4      1        2     3        2        0      4       0 49.0
    #> 9        5        0      1        2     0        4        3      6       4 33.5
    #> 10       3        6      1        2     1        5        4      4       0 55.0
    #> 11       4        0      4        5     7        3        6      1       5 60.5
    #> 12       0        0      7        7     5        5        7      5       1 60.0
    #> 13       1        3      4        6     7        6        1      7       1 77.0
    #> 14       0        3      5        3     0        4        4      5       4 37.5
    #> 15       4        5      5        7     5        3        7      0       2 70.5
    #> 16       0        5      4        6     1        6        1      1       5 54.0
    #> 17       3        6      6        7     7        3        6      0       1 80.0
    #> 18       0        6      3        1     3        0        3      6       2 38.5
    #> 19       0        3      3        5     3        0        5      7       1 35.0
    #> 20       4        5      2        7     5        3        4      7       7 69.5
    #> 21       3        6      6        3     5        3        6      0       4 68.0
    #> 22       3        6      5        4     1        7        6      5       0 70.5
    #> 23       1        2      3        1     3        6        2      3       1 50.5
    #> 24       6        2      3        1     5        1        6      7       5 52.5
    #> 25       0        0      0        0     0        0        0      0       0  0.0
    #> 26       1        1      1        1     1        1        1      1       1 16.0

The output shows the original data.frame but with recoded food groups
consumption frequency and with a calculated food consumption score
(`fcs` variable).

Finally, to classify the food consumption scores into respective FCS
classifications using the
[`fcs_classify()`](https://nutriverse.io/dietry/reference/fcs_classify.md)
function as follows:

``` r
## Create a variable map to match food group labels to variables in fcs01 ----
var_map <- fcs_fg_map_variables(
  staples = "FCSStap",
  pulses = "FCSPulse",
  vegetables = "FCSVeg",
  fruits = "FCSFruit",
  meatfish = "FCSPr",
  milk = "FCSDairy",
  sugar = "FCSSugar",
  oil = "FCSFat",
  condiment = "FCSCond"
) 

## Calculate scores ----
fcs_df <- fcs_calculate(df = fcs01, var_map = var_map, add = TRUE)

## Classify scores ----
fcs_classify(fcs = fcs_df$fcs, add = TRUE)
```

which gives the following output:

    #>     fcs  fcs_class
    #> 1  49.0 acceptable
    #> 2  40.0 acceptable
    #> 3  45.0 acceptable
    #> 4  60.5 acceptable
    #> 5  31.0 borderline
    #> 6  53.5 acceptable
    #> 7  32.0 borderline
    #> 8  49.0 acceptable
    #> 9  33.5 borderline
    #> 10 55.0 acceptable
    #> 11 60.5 acceptable
    #> 12 60.0 acceptable
    #> 13 77.0 acceptable
    #> 14 37.5 acceptable
    #> 15 70.5 acceptable
    #> 16 54.0 acceptable
    #> 17 80.0 acceptable
    #> 18 38.5 acceptable
    #> 19 35.0 borderline
    #> 20 69.5 acceptable
    #> 21 68.0 acceptable
    #> 22 70.5 acceptable
    #> 23 50.5 acceptable
    #> 24 52.5 acceptable
    #> 25  0.0       <NA>
    #> 26 16.0       poor

A new data.frame is returned with the `fcs` variable and the `fcs_class`
variable which indicates whether the household has a *poor*,
*borderline*, or *acceptable* food consumption score.
