# brto-llvm
Modern C++ implementation of the Kaleidoscope LLVM tutorial.

## Getting Started
### Prerequisites
* [CMake](http://cmake.org) 2.8 or later
* [LLVM](http://releases.llvm.org/)
* A C++1X compiler

### Building
Clone or download the repository to a local folder of your choice and
create a separate build directory, e.g.:
```
mkdir build && cd build
```
Generate the project files, e.g.:
```
cmake ../ -DCMAKE_BUILD_TYPE=Debug -DCMAKE_CXX_COMPILER=clang++
```
Build the project, e.g.:
```
make
```

## Usage
Run the produced executable, e.g.:
```
./brto
```

## Built with
* [C++](http://isocpp.org)
* [CMake](http://cmake.org)
* [LLVM](http://llvm.org)

## References
* [Kaleidoscope tutorial](https://llvm.org/docs/tutorial/index.html)
* [Design Patterns by the gang of four](https://en.wikipedia.org/wiki/Design_Patterns)

## Tested platforms and compilers
* ArchLinux, kernel 4.12 x86_64, clang++ 4.0.1

## Authors
* **Alberto Taiuti** - *Developer* -
[@snowzurfer](https://github.com/snowzurfer)

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file
for details
