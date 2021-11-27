DeckBuilder
-----------
DeckBuilder is a deck building game simulator that allows you,
the Haskell programmer, to defined new games and AI
schemes.

You then run the simulator by defining number of games to
play, how many players, and which AIs to pit against each other.

Goals
-----
I am writing the documentation for this program to help
Haskell beginners.

Language Extensions
-------------------
Language extensions that are used throughout the entire
project can be configured in Stack or Cabal's configuration
file, so this list isn't necessarily all the active extensions
being used for the file).

DeriveDataTypeable (https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_extra.html)
We use the deriving functionality for command line argument parsing
using System.Console.CmdArgs.

> {-# LANGUAGE DeriveDataTypeable        #-}

OverloadedStrings (https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_strings.html)
There are multiple representations of strings of characters in
Haskell (I know, I know, we're working on it, but some of it
is just the nature of how software works). When you use a
string literal like "ahoy ahoy", by default it is of type
String, but this is not a type that will work for anything
non-trivial.

The extension changes it to be an instance of @IsString@,
so the compiler can figure out the right choice based on
context and you, the __Haskell Programmer_, don't have to
constantly write conversion code.

> {-# LANGUAGE OverloadedStrings         #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE ScopedTypeVariables #-}

Module declaration
------------------
The Main module exists to export the @main@ function,
which is the entry point to the Haskell program by the
Haskell Runtime System.

> module Main ( main ) where

Imports
-------
After the Module declaration, but before the
functions that make it up, we have the list of
modules (and, optionally, specific functions) we're
using in this module from another one.

> import DeckBuilding.Dominion
>    ( randomKingdomDecks, runGames )
> import DeckBuilding.Dominion.Cards ( kingdomCards2ndEdition )
> import DeckBuilding.Dominion.Strategies.Basic
>    ( bigMoneyStrategy, bigSmithyStrategy, villageSmithyEngine4 )
> import DeckBuilding.Dominion.Types
>    ( DominionConfig(DominionConfig) )
> import Prettyprinter
>    ( layoutPretty,
>      vsep,
>      defaultLayoutOptions,
>      pretty )
> import Prettyprinter.Render.Text ( renderStrict )

Qualified Imports
-----------------
Here we're importing Data.Text with a prefix, in
this case Text, so any function within that module
can be addressed with Text.funcName, such as
Text.length.

> import qualified Data.Text as Text
> import System.Random ( newStdGen, StdGen, mkStdGen )
> import System.Console.CmdArgs
>    ( Data,
>      Typeable,
>      (&=),
>      cmdArgs,
>      details,
>      help,
>      summary,
>      verbosity,
>      typFile )
> import Test.QuickCheck (generate, Gen, arbitrary)
> import DeckBuilding.Dominion.Types (DominionGame)

Instance importing
------------------
This style of importing is for telling the
commpiler where to find some typeclass instances
that are used here. In this case we need to
have access to the Pretty instances for the
data structures that represent what happened
in a game.

> import DeckBuilding.Dominion.Pretty ()

Command line parsing
--------------------
Using the cmdargs package (https://hackage.haskell.org/package/cmdargs) for command line
parsing.

First we have the data structure that holds the
parsed values.
  times - number of games to run
  out - file name to put the results in

> data DeckBuilder = DeckBuilder
>  { times :: Int
>  , out :: FilePath
>  , seed :: Maybe Int
>  , prettyTest :: Bool
>  }
>  deriving (Data, Typeable, Show, Eq)

Parsing function
----------------
This is the function that gives the data structure
for the cmdArgs parser.

> deckBuilder :: DeckBuilder
> deckBuilder = DeckBuilder
>  { times = 1 &= help "Number of games to run."
>  , out = "deckgames.txt" &= typFile &= help "File name to write the game logs to, default is 'deckgames.txt'"
>  , seed = Nothing &= help "Base seed for the entire run, default is RNG."
>  , prettyTest = False &= help "Use QuickCheck to test pretty printing output by throwing random game results at it."
>  } &= 
>  verbosity &=
>  help "stack run -- deckbuilder-exe --times 1 --out myfile.txt" &=
>  summary "DeckBuilder version 0.1.0.2, (C) Andrew Boardman" &=
>  details [ "DeckBuilder runs a simulation of deck building games."
>          , "You can create new strategies and run them against each other."
>          , "See the README.md for instructions!"
>          ]

main
----
Here is where the Haskell program starts. The return type
is IO (), which means this portion of the program is able
to do essentially unrestricted Input and Output, and when
it exits there is no value returned.

> main :: IO ()

Do notation
-----------
Think of do notation as an easier way to string a series
of function calls together that should go in more or less
a particular order. You can do this with operators instead,
but that often makes the code harder to read.

> main = do

  Parse the command line

>  args' <- cmdArgs deckBuilder

  If the user has specified a seed on the command line,
  use it. Otherwise, use a pseudo-random number generator to create an
  initial random seed.
   
>  g <- case seed args' of
>          Nothing -> newStdGen
>          Just n -> return $ mkStdGen n

  Get the number of runs from the command line --}

>  let n = times args'

  Create a configuration for the Dominion game.
  I'd like this to all be specified on the command line
  or in a config file at some point (probably a combo of
  the two).
  
  DominionConfig is the constructor for the data
  structure of the same name, and it contains the list
  of players (name, strategy), and the Kingdom cards
  for this game.
  
>      conf = DominionConfig
>              [ ("Big Money", bigMoneyStrategy)
>              , ("Big Smithy", bigSmithyStrategy)
>              , ("Village/Smithy Engine 4", villageSmithyEngine4)
>              ]
>              (randomKingdomDecks kingdomCards2ndEdition g)

  runGames does the actual work of simulating all
  of the games, given the number requested, the configuration,
  and the random number seed.

  It returns a list of Doc ann, which is the pretty
  printed results. It then vertically separates them.

>  arbs :: [DominionGame] <- generate arbitrary
>  let res = if prettyTest args'
>              then pretty arbs
>              else vsep $ runGames n conf g

  Take that Doc ann and lay it out, then write it
  to the output file.

>  (writeFile (out args') . Text.unpack . renderStrict . layoutPretty defaultLayoutOptions) res
