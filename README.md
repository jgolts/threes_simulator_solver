[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
![R Version](https://img.shields.io/badge/R-4.5.0-blue)

A simulation that determines winrates for optimal, random, and anti-optimal strategies in the dice game Threes. Includes a function that determines best combination of dice to save at a given point in the game. The function in question is `gen_min_set`, and to use it, you supply a vector of dice rolled on that turn (e.g. `c(2, 3, 5)`), and then the number of dice saved/out of play, and the sum of their values.
