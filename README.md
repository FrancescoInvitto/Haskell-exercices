# Haskell-exercises
A collection of Haskell exercises developed during the "Paradigmi e linguaggi per l'analisi dei dati" (Paradigms and languages for data analysis) course of master degree at University of Parma.

# How to execute Haskell code
In order to execute the Haskell programs you can install Haskell on your machine or use an online tool (I used Replit).

## Install Haskell (Linux system)
Follow these steps:
  1. set up Haskell environment: go to the official website and select your distribution, you will get a page showing the command to install Haskell
  2. open your terminal and write the command (example with Ubuntu)
  ```console
    sudo apt-get install haskell-platform
  ```
  3. launch the ghci command; once you get the Prelude prompt you are ready to use Haskell
  ```console
  ghci
  ```
  4. compile your code
  ```console
  ghc -o filename filename.hs
  ```
  5. run the executable
  ```console
  ./filename
  ```

## Using Replit
Follow these steps:
  1. save your code with .hs extension.
  2. load the file in the console
  ```console
  :l filename.hs
  ```
  3. write the name of the function you want to execute
  ```console
  function param1 param2
  ```

