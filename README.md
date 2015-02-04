# 2048 game clone using Yampa FRP library

After trying to grasp the idea of FRP (mostly concentrating on Yampa library) I
have finally found myself understanding it enough to write something simple.

![yampa-2048 using Gloss](http://ksaveljev.github.io/2048.gif)

This project is based on some code by other people. I wasn't interested in
implementing the logic of 2048 myself or drawing to Gloss window from scratch.
As a result this repository contains some chunks of the code written by other
people:

[Josh Kirklin](https://github.com/ScrambledEggsOnToast) and his [excellent
implementation of 2048 in Elm](https://github.com/ScrambledEggsOnToast/2048-elm)
 provided me with the game logic.

[Maia Werbos](https://github.com/tigrennatenn) and her [great implementation of
2048 using Gloss](https://github.com/tigrennatenn/2048haskell) provided me
with the rendering chunk of code.

I was able to come up with my solution after reading the code by 
[Keera Studios](https://github.com/keera-studios) and their [amazing Haskanoid 
project](https://github.com/ivanperez-keera/haskanoid)

Running:

    cabal sandbox init
    cabal install --dependencies-only
    cabal run

The gameplay is pretty simple. Nothing fancy. Try to survive for as long as
possible.

Things I would like to change but probably won't:
- Gloss lacks the ability to style the font, therefore those numbers don't look
  nice but as long as works it seems to be ok
- I wanted the game to exit upon the Esc button press but didn't bother to
  investigate why it is not closing or how to do it
