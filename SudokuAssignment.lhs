Sudoku
======

For this practical, you will need the following file:

* [Sudoku.hs](Sudoku.hs)

Sudoku is a game that is often found in British newspapers. The game
comprises of 81 boxes that are to be filled in with numbers between
`1` and `9`. The goal is to have a different number on each row,
column, and each of the nine `3x3` blocks.

*****************************************************************************
**TODO:** If you haven't played the game before, then you should have a go
at [http://www.sudoku.com/](http://www.sudoku.com/) whatever you do, 
don't spend more than 5 minutes on this task. I hear it's addictive.

*****************************************************************************

Our goal for this assignment is to build a simple Sudoku solver. We
won't be heading for a super efficient version, but something that
can solve the hardest of puzzles in under 10 seconds or so.

We will represent the input format for a Sudoku puzzle as a string
of length 81 where the characters 1 to 9 represent the numbers, and
a space represents a blank square. Here's an easy puzzle that we'll
try to create a solver for:

< easy :: String
< easy =
<   " 4   2 19\
<   \   351 86\
<   \31  947  \
<   \ 94     7\
<   \         \
<   \2     89 \
<   \  952  41\
<   \42 169   \
<   \16 8   7 "

This string makes it easy to input a new puzzle into the program we
are building, but it's not a good representation for manipulating
potential solutions.

One function that will be very useful for manipulating lists is
`bundle`, with the following signature:

< bundle :: Int -> [a] -> [[a]]

The result of `bundle n xs` is to bundle the list `xs` up into lists 
of size `n`. 

For instance, the result of `bundle 3 [1 .. 10]` should be:

    [[1,2,3],[4,5,6],[7,8,9],[10]]

It should have the following properties:

< concat (bundle n xs) == xs
< length xss > 1 ==> all (\xs -> n == length xs) (init xss)

These properties are used in my testing rig to check if your
implementation is correct. Make sure you can understand what they
are saying.

*****************************************************************************
**TODO #S1:** Define the `bundle` function in `Sudoku.hs`

**NOTE:** All the code you modify should be placed in the `Sudoku.hs` file.
This file contains some definitions on lines that start with `>`.
Lines that start with `<` don't have a special meaning in Haskell:
they're just here to make the assignment more readable.

*****************************************************************************

To work our way to a solution, it will help to think of each cell as
containing a number of potential choices we can make. Thus, if a
value has been given from the outset, there is no choice and the
cell should contain just that value. If it is blank, then we can
choose any of the 9 values. If the game is stuck, that is, if there
are no legal moves that can be made in a cell, then it should be
empty.

Here is some code that introduces a new datatype called `Sudoku`,
and a function that converts a `String` into a `Sudoku`.

< type Cell = [Char]
< data Sudoku = Sudoku [Cell]

The definition of `Cell` is a type synonym, which means that wherever
you see `Cell` in the following functions, then this can be replaced
directly with `[Char]`.

The definition of `Sudoku` introduces a new *constructor* called
`Sudoku` that given a list of cells will produce a value of type
`Sudoku`. Notice that we're giving the same name to both the
type we are introducing, on the left-hand-side of the definition,
and the constructor we are introducing, on the right-hand-side of the
definition. This is fairly standard practice.

We use this type in the following definition, which
converts a `String` into a value of type `Sudoku`.

< sudoku :: String -> Sudoku
< sudoku xs = Sudoku (map convert xs)
<   where convert x = if x == ' ' then ['1'..'9'] else [x]

This function converts the values in `xs` into either a single value,
or expands it into a list of possible options if it is a blank
character. Once the list has been converted, it is used as a parameter
to `Sudoku` to construct a value of type `Sudoku`.


*****************************************************************************
**TODO:** Load your `Sudoku.hs` into `ghci` and test out the `sudoku`
function:

    $ ghci Sudoku
    GHCi> sudoku easy

This should print out the following in your terminal, if it doesn't
then make sure you implement `bundle` properly, since it's used
in the definition of the function that displays:

    ╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗
    ║   │ 4 │   ║   │   │ 2 ║   │ 1 │ 9 ║
    ╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
    ║   │   │   ║ 3 │ 5 │ 1 ║   │ 8 │ 6 ║
    ╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
    ║ 3 │ 1 │   ║   │ 9 │ 4 ║ 7 │   │   ║
    ╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣
    ║   │ 9 │ 4 ║   │   │   ║   │   │ 7 ║
    ╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
    ║   │   │   ║   │   │   ║   │   │   ║
    ╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
    ║ 2 │   │   ║   │   │   ║ 8 │ 9 │   ║
    ╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣
    ║   │   │ 9 ║ 5 │ 2 │   ║   │ 4 │ 1 ║
    ╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
    ║ 4 │ 2 │   ║ 1 │ 6 │ 9 ║   │   │   ║
    ╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
    ║ 1 │ 6 │   ║ 8 │   │   ║   │ 7 │   ║
    ╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝

If you want to see the code that was used to produce this, then you'll
find it in the `Sudoku.hs` file.

*****************************************************************************

The strategy we will use for our solver is given by the following
function called `solve`.
The function returns a list of possible answers to solving a Sudoku
puzzle. Good puzzles only ever have one answer, but we'll not
make that assumption here.

< solve :: Sudoku -> [Sudoku]
< solve puzzle
<   | stuck puzzle || not (valid puzzle) = []
<   | fixed puzzle = [puzzle]
<   | otherwise    = [puzzle'' | puzzle'  <- expand puzzle
<                              , puzzle'' <- solve puzzle']

I've provided you with the definition of `solve` and it relies
on a number of other functions. Once you define those other functions
fully, this `solve` function will work. If you try it now,
you'll get an error that says that things are `undefined`.

The `solve` function checks to see if the game is stuck or not valid.
If so, then we return no solutions by giving back an empty list.
If the puzzle is fixed then it returns just the puzzle.
Otherwise, we expand the puzzle to get a list of possibilities, and
then we recursively try to solve the puzzle.

I have left the definitions for `stuck` and `fixed` as undefined.

Perhaps the easiest to define is `stuck`. We'll say that a game is
stuck if there are any cells in the game that are empty. You can check
if a list is empty by using the `null`
function from the `Prelude`:

< null :: [a] -> Bool

The definition body of `stuck` should only be one line long if you
make use of the `any` function:

< any :: (a -> Bool) -> [a] -> Bool

The result of `any p xs` checks to see if any of the values in the
list `xs` satisfy the predicate `p`.

*****************************************************************************
**TODO #S2:** Define the `stuck` function in `Sudoku.hs`.

*****************************************************************************

The `fixed` function is very similar: it detects when a game is
finished. Think about what it means for a game to be complete,
in terms of the number of choices provided in each cell.
Note that you don't need to check to see if the game is valid, since
the pattern match in `solve` does this beforehand.

I've provided the `lone` function which will be useful for this task.

< lone :: [a] -> Bool
< lone [x] = True
< lone xs  = False

This checks to see if a list consists of a lone element.

*****************************************************************************
**TODO #S3:** Define the `fixed` function in `Sudoku.hs`. Make sure your
definition uses `lone`, that checks if a list contains only one value.

*****************************************************************************

The `valid` function checks to see if a game is consistent:
all of the rows, columns, and boxes whose cells are lone should be
unique.

< valid :: Sudoku -> Bool
< valid puzzle = all (unique . filter lone) (rows puzzle)
<             && all (unique . filter lone) (cols puzzle)
<             && all (unique . filter lone) (boxs puzzle)

This is the full definition, but I've left the functions `rows`,
`cols` and `boxs` unimplemented.
Your task is to define these. These are in increasing order of difficulty.
You will probably want to make use of the `transpose` function from
the previous practical.

*****************************************************************************
**TODO #S4:** Define the `rows`, `cols` and `boxs` functions in `Sudoku.hs`.
Extra marks will be awarded for elegant definitions.

*****************************************************************************

Aside: I'm sure you're wondering why `boxs` is spelled that way. I like it
when my functions have the same number of characters. I also like
adding an `s` on the end of singular things to make them plural.
Sometimes I bend the rules of English a little to make things fit.
Besides, you probably didn't complain when I missed out the "umn" in
"columns".

The last piece of the puzzle is the `expand` function. The idea
here is to take a Sudoku and expand a single cell that contains multiple options,
thus creating a list of possible Sudokus.

< expand :: Sudoku -> [Sudoku]
< expand (Sudoku cells) = [ Sudoku (ls ++ [choice] : rs) | choice <- choices ]
<   where
<     (ls, choices:rs) = break (not . lone) cells

This chooses to expand the first cell that contains choices.

If you completed `rows`, `cols` and `boxs`, then you have assembled
everything that's needed to solve a Sudoku puzzle. Have a go in your
terminal!

    GHCi> solve (sudoku easy)

This should take a few seconds. If it works, well done! Otherwise,
you have something left to debug.

Compiled Code
-------------

The Sudoku solver we have constructed works fine, but is really rather
slow. For example, try doing `solve (sudoku hard)`. It'll take quite
a long time.

One way to speed things up somewhat is to run a compiled version of
your code, rather than the interpreted version in GHCi.

    $ ghc Sudoku.hs -main-is Sudoku
    $ ./Sudoku

This has been hard-coded to try and solve the `hard` problem,
but you can also feed it puzzles as text files. For instance,
download [hard.txt](hard.txt), and try:

    $ ./Sudoku hard.txt

This returns a first answer in under 10 seconds on my machine,
but then it takes a long time to figure out that there are
no other solutions.

Extension
=========

For additional work, there are plenty of options to explore. You can
choose to do some of the following:

* Investigate the performance of our Sudoku solver in detail,
  and make it go faster. You should read the RWH section on
  [profiling and optimization](http://book.realworldhaskell.org/read/profiling-and-optimization.html),
  and apply what is there.

* Investigate and solve some of the variations of Sudoku,
  such as Killer Sudoku.

* Implement a tool that generates Sudoku puzzles and indicates
  how hard they are. You'll have to think of a good measure
  of what "hard" means.

* You could alternatively attempt to solve some other puzzle.

