# TMCRTracker

A tracker for the randomizer for The Legend of Zelda: The Minish Cap.

## Usage

To build the project, you will need [stack](https://docs.haskellstack.org/en/stable/README/).

Then, simply execute `stack run` in the project directory.
This will start the tracker, which you can access using your browser on [localhost:8023](localhost:8023).

There are no programm options yet (although I am planning on adding some).

##Quirks

Unlike other trackers keys in dungeons are tracked even without keysanity, so locations might not show as being in logic even when you will be able to reach them for sure (i.e. temple of droplets will only show the first location in logic until you mark keys). These are positioned with the dungeon locations, not the other items.

Every item that has logic rules is shown. This includes all types of rupees and an item called Untyped.FF which is used to make locations inaccessible.

You can mark any number of any item, irrespective of the number available in the game.

##Planned Features and Improvements

 - Improve logic file choice mechanism (currently hardcoded)
 - Some better way of handling settings choices
 - Item icons
 - locations shown on a map
 - maximal amounts for items
 - possibly: show logic requirements for locations
