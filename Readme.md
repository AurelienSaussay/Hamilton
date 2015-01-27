# Readme

Hamilton is a Mathematica package to automatically derive the first order conditions (FOC) of a continuous dynamic optimization problem of the variety commonly encountered in economics.

## Installation

1. Download Hamilton.m
2. In Mathematica, click File -> Install
3. Select `Package` as the type of item to install, and `From File` as the package source, selecting the file Hamilton.m
4. In your Notebook, insert ``<< Hamilton` `` to use the package (note the final backtick !)

## Documentation

The package contains a single function, Hamilton.

`Hamilton[`_`objective`_`, `_`constraints`_`, `_`Output`_` -> `**`"Full"`**`, `_`Multipliers`_` -> `**`{}`**`]`

- **`objective`**: an equation representing the objective to be maximized. Should be expressed in current value.
- **`constraints`**: a list of equations representing the constraints applied to the problem. Can be equalities or inequalities.
- **`Output`** _(optional)_: parameter controlling the type of output. Possible values include:
    * **`"Full"`** _(default)_: fully formated maximization problem, along with its FOCs.
    * **`"Hamiltonian"`**: the present value Hamiltonian of the problem.
    * **`"FOC"`**: the first order conditions, returned as a list.
- **`Multipliers`** _(optional)_: optional list of variables, to be used to name the co-state variables instead of the default choices. Must be of the same length as the number of constraints.

## Examples

- Full output and default multipliers:

![insimple](https://cloud.githubusercontent.com/assets/484048/5918648/81322d80-a62b-11e4-85fe-3eddee128241.png)

_Output_

![outsimple](https://cloud.githubusercontent.com/assets/484048/5918654/81478ee6-a62b-11e4-8d84-e89efa5f0241.png)

- Returning only the Hamiltonian:

![insimplehamiltonian](https://cloud.githubusercontent.com/assets/484048/5918652/813933f0-a62b-11e4-858b-34e711e9350b.png)

_Output_

![outsimplehamiltonian](https://cloud.githubusercontent.com/assets/484048/5918656/81493552-a62b-11e4-82b5-7a8cfd88c6d8.png)

- Returning a list of the FOCs:

![insimplefoc](https://cloud.githubusercontent.com/assets/484048/5918650/8136bdd2-a62b-11e4-9278-cac2d686f867.png)

_Output_

![outsimplefoc](https://cloud.githubusercontent.com/assets/484048/5918657/814c582c-a62b-11e4-9a96-0c7d47cf6a8b.png)

- Using custom multipliers:

![insimplecustommult](https://cloud.githubusercontent.com/assets/484048/5918651/8137684a-a62b-11e4-9c15-cbbd132823be.png)

_Output_

![outsimplecustommult](https://cloud.githubusercontent.com/assets/484048/5918655/81484e30-a62b-11e4-8f18-9d869d8958de.png)

- Using indices (experimental):

![inindices](https://cloud.githubusercontent.com/assets/484048/5918649/81349980-a62b-11e4-9f41-1d90d5a769d2.png)

_Output_

![outindices](https://cloud.githubusercontent.com/assets/484048/5918653/813a0366-a62b-11e4-9597-bcdc8cad9768.png)