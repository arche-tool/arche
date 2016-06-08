# GammaBuilder #

GammaBuilder is a software tool for the reconstruction of the parent austenite (before phase transformation) in steels.

### Installation ###

GammaBuilder is written in Haskell and the visualisation is done in [Paraview](http://www.paraview.org/) (VTK files). Therefore the following softwares need to be installed:

* [stack](http://docs.haskellstack.org/en/stable/install_and_upgrade/#installupgrade)
* [Paraview](http://www.paraview.org/download/)

Once the stack is installed, open a terminal (cmd.exe in Windows), go to a folder where the files will be placed on and then run the following commands:

```
#!bash

git clone git@bitbucket.org:lostbean/gammabuilder.git

cd gammabuilder

git submodule update --int

stack install

```

In order to update to the latest modifications, go the folder that contains the source files and run the following commands:

```
#!bash

cd gammabuilder

git pull
git submodule update --int

stack install

```

Note that the internet access is required in order to download all the other dependencies.
