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

Simple example with Full output and default multipliers:

``

Hamilton[u[c[t]],
  {F[Subscript[q, p][t], Subscript[k, c][t]] ==
    c[t] + Subscript[i, p][t] + Subscript[i, c][t],
   Subscript[k, p]'[t] ==
    Subscript[i, p][t] - \[Delta] Subscript[k, p][t],
   Subscript[k, c]'[t] ==
    Subscript[i, c][t] - \[Delta] Subscript[k, c][t],
   m'[t] == G Subscript[q, p][t] - \[CurlyEpsilon] m[t],
   m[t] <=
\!\(\*OverscriptBox[\(m\), \(_\)]\),
   Subscript[i, p][t] >= 0,
   Subscript[q, p][t] <= Subscript[k, p][t]
   }, Multipliers -> {\[Lambda], \[Nu], \[Chi], \[Mu], \[Phi], \
\[Psi], \[Beta]}] // TraditionalForm
``
