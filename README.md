# What is Anytool

anytool is a cli program to generate various channel lists for ham radio / pmr446 / Freenet for the Anytone AT-D878UVIIplus.

# Install

* Install haskell platform via [ghcup](https://www.haskell.org/ghcup/)
* clone repository
* build && install
```console
foo@bar:~$ git clone http://github.com/polyrod/anytool
foo@bar:~$ cd anytool
foo@bar:anytool$ cabal build
foo@bar:anytool$ cabal install
```
# Usage

* From your AnytoneCPS export channels.csv
* remember your highest assigned channel
* run anytool to generate channels you want
  ```console
  foo@bar:~$ anytool --all -n <first free channel number above your channels> >> channels.csv
  ```
* ReImport channels.csv into your AnytoneCSP

# Help

```console
foo@bar:~$ anytool --help 

DC1MDP Maurizio Di Pietro © 2024-- Use as is. I take no responsibility for any
inconvieniences you may encouter using this tool.

Usage: anytool [[-7|--70cm] [-r|--r70cm] [-2|--2m] [-R|--r2m] [-4|--pmr446] 
                 [-f|--freenet] [-s|--sat] |
                 (-a|--all)] [-n|--channel-number INT]

  Anytool -- Utility to generate various channel and satellite data for the
  Anytone AT-D878UVIIplus

Available options:
  -7,--70cm                generate 70cm fm simplex channels
  -r,--r70cm               generate 70cm fm repeater channels
  -2,--2m                  generate 2cm fm simplex channels
  -R,--r2m                 generate 2m fm repeater channels
  -4,--pmr446              generate pmr fm simplex channels
  -f,--freenet             generate pmr fm simplex channels
  -s,--sat                 generate fm sat tle file (./anytool.tle)
  -a,--all                 generate all data
  -n,--channel-number INT  Channelnumber to start, should be above the latest of
                           your programmed channels (default: 2000)
  -h,--help                Show this help text
```
