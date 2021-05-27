# EquationGenerator
An equation generator written in SICStusProlog (subset of Prolog). 

Created with my fellow students Rami Karim and Albin Johansson as a course project in the course Logic Programming at the Royal Institute of Technology (KTH).

## How to use the program
When generating the first equation, call the clause `generate_eq\2`.

`generate_eq(+Env, -Equation)`:
- `Env`: The user sends in the variable environment in the form of lists within a list, e.g. `[ [x, 1], [y,2] ]`, which means x = 1` and `y = 2`. The user needs to specify at least one binding.

- `Equation`: The equation generated will be bound to this variable in the form of a list, e.g `[24, =, 3, x, +, 4, y]`.

When trying to generate additional equations (in case the user wants equations they can put inside an equation system), the user calls the clause `generate_eq\3`.

`generate_eq(+Env, +Equation_list, -Equation)`:
- `Env`: Same as above, the user sends in the variable environment. Note that the environment for this equation has to match the environment used to generate the equations in `Equation_list `or else the program will fail.
- `Equation_list`: A list of equation that has been generated with this program. The list of equations are stored inside a list, e.g `[ [24, =, 3, x, +, 4, y, +, z ], [56, =, 8, x, +, 9, y, + z] ]`. 

**Note** that all the equations in the list has to be linearly independent with each other. Also note that if the environment has N amount of variables, the `equation_list` can only contain a maximum of N-1 equations. This is because the program can only be used to generate equations for an equation system that has the same amount of equations as variables. 

If the user is not pleased with the equation presented, they can simply reply “n” to the presented binding and the program will generate a different equation for the user



