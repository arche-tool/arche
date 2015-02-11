# GammaBuilder #

GammaBuilder is a software tool for the reconstruction of the parent austenite (before phase transformation) in steels. 

### Installation ###

GammaBuilder is written in Haskell and the visualisation is done in [Paraview](http://www.paraview.org/) (VTK files). Therefore the following softwares need to be installed: 

* [ghc](https://www.haskell.org/platform/)
* [Paraview](http://www.paraview.org/download/)

Once the ghc is installed, open a terminal (cmd.exe in Windows), go to a folder where the files will be placed on and then run the following commands:

```
#!bash

git clone git@bitbucket.org:lostbean/gammabuilder.git
git clone git@bitbucket.org:lostbean/hammer.git
git clone git@bitbucket.org:lostbean/sledge.git
git clone git@bitbucket.org:lostbean/queryforest.git

cd gammabuilder
cabal update
cabal sandbox init
cabal sandbox add-source ../Hammer ../sledge ../queryforest

cabal install

```
Note that the internet access is required in order to download all the other dependencies.

### How do I get set up? ###

* Summary of set up
* Configuration
* Dependencies
* Database configuration
* How to run tests
* Deployment instructions

### Contribution guidelines ###

* Writing tests
* Code review
* Other guidelines

### Who do I talk to? ###

* Repo owner or admin
* Other community or team contact