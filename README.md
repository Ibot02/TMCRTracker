#TMCR Tracker#

A tracker for the randomizer for The Legend of Zelda: The Minish Cap

You can [try this tracker online](https://ibot02.github.io/TMCRTracker/).

Alternatively, a version that runs a local web server on port 3003 as well as an android version is available as [releases](https://github.com/Ibot02/TMCRTracker/releases).  
These versions have improved performance over the javascript version.

##Building from source##

First, clone this repository as well as the `reflex-platform` submodule.

```
   git clone https://github.com/Ibot02/TMCRTracker.git
   cd TMCRTracker/reflex-platform
   git submodule init
   git submodule update
   cd ..
```

You will need [nix](https://nixos.org/nix/) to build this project.
A script to install nix as well as some other dependencies is included with reflex-platform.

```
   ./reflex-platform/try-reflex
   exit
```

This will install nix and open a nix shell.

At this point, you can build using one of the following:

```
   nix-build -A ghcjs.frontend -o outputDirectory #javascript version
   nix-build -A ghc.frontend -o outputDirectory #local web server version
   nix-build -A android.frontend -o outputDirectory #android app
   nix-build -A ios.frontend -o outputDirectory #ios app
```

Android apps can only be build on linux, and ios apps can only be build on macOs.

##Planned Features##

 - The ability to select multiple locations from the map at once
 - The ability to view the logic requirements for a location
 - Saving/Loading state
 - Display changes to Pieces of Heart / Heart Containers
