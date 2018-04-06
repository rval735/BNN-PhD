# NNClash

Simple neural network that analyzes MNIST data in CSV format but targets FPGA
boards using Clash.

To compile, simply clone the repository and run (within the folder):

```bash
stack build
stack exec clash MAC.hs -- "--vhdl"
```

That will read the file "MAC.hs", then compile and translate to "vhdl" code. Consider that "clash" should be already installed using ```cabal install clash-ghc``` or using the binary compiled form stack.