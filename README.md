# Automated_Reasoning
Code and resources for "Handbook of Practical Logic and Automated Reasoning" OCaml 3.09.1

## Installation of OCaml 3.09.1
Install the Opam package manager 
```bash
bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
```
Initialize Opam
```bash
opam init
```
If your Linux distributioun was to Lite and your Compiler trows an error like this:
```bash
error: C compiler cannot create executables
```
You probably need to Install the following package
```bash
sudo apt install libc6-dev
```
Install OCaml 3.09.1
```bash
opam switch create 3.09.1
```
switch to OCaml 3.09.1
```bash
eval $(opam env --switch=3.09.1)
```

