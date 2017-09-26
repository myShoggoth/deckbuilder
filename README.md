# deckbuilder

A deck-building game simulator engine. I'm writing simulators for multiple
games of the genre and hoping to extract a general structure customized per
game.

First game is Dominion (https://boardgamegeek.com/boardgame/36218/dominion),
the creator of the genre. So far I've implemented the cards of the 2nd edition
of the basic game.

How to get it up and running:

1. Clone the repository
2. Install the stack tool (https://docs.haskellstack.org/en/stable/README/)
3. Change to the root directory for the repository and use `stack build`.
4. Once it is built, you can run `stack exec -- deckbuilder-exe` to try it out.
5. To run unit tests: `stack test`.

To create a new playing strategy:

1. Create the Haskell file in src/DeckBuilding/Strategies/
2. Implement the functions for the Strategy type, see
  src/DeckBuilding/Dominion/Types.hs for details.
3. Try it out/debug it interactively with `stack ghci`
4. Change app/Main.hs to use your strategy.
5. If you do and you like it, submit a MR!

To implement the cards for an expansion:

1. Create a Haskell file in the src/DeckBuilding/Dominion/Cards directory
  called ExpansionName.hs.
2. Implement the cards, see src/DeckBuilding/Dominion/Cards/Base.hs for lots of
  examples and helper functions.
3. If you need to make data or algorithm changes, let me know so we can discuss
  design ideas.
4. Have your module publish the cards, the list of cards, etc.
5. Write unit tests.
6. Submit a MR!
