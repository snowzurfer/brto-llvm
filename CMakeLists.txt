# MIT License
#
# Simple compiler front-end for LLVM written as a learning exercise.
# Copyright © 2017 Alberto Taiuti
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
# OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
# DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

cmake_minimum_required(VERSION 2.8)

project(brto-llvm)

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
  message(STATUS "Build mode: debug")
else()
  message(STATUS "Build mode: release")
endif (CMAKE_BUILD_TYPE STREQUAL "Debug")

# Export commands for YCM
set(CMAKE_EXPORT_COMPILE_COMMANDS 1)

# Set custom cmake modules path
set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules")

# Set default cmake build type
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug CACHE STRING
      "Choose the type of build, options are: None Debug Release RelWithDebInfo\
MinSizeRel. Default is Debug."
      FORCE)
endif(NOT CMAKE_BUILD_TYPE)

if(MSVC)
  # Force to always compile with W4
  if(CMAKE_CXX_FLAGS_DEBUG MATCHES "/W[0-4]")
    string(REGEX REPLACE "/W[0-4]" "/W4" CMAKE_CXX_FLAGS_DEBUG
           "${CMAKE_CXX_FLAGS_DEBUG}")
  else()
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /W4")
  endif()
elseif(CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR
   CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  # Update if necessary
  set(CMAKE_CXX_FLAGS
    "${CMAKE_CXX_FLAGS} -std=c++1z -Wall -Wextra -stdlib=libc++ -fno-rtti")
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -O0")
  set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -O0")
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} -O2 -g")
  set(CMAKE_C_FLAGS_RELWITHDEBINFO "${CMAKE_C_FLAGS_RELWITHDEBINFO} -O2 -g")
endif()

if(UNIX)
  if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    # Color error output
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fcolor-diagnostics")
  endif()
endif()

message(STATUS "Compiler custom flags are: ${CMAKE_CXX_FLAGS}")
message(STATUS "Compiler debug flags are: ${CMAKE_CXX_FLAGS_DEBUG}")
message(STATUS "Compiler release flags are: ${CMAKE_CXX_FLAGS_RELEASE}")

# Find and use LLVM
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
message(STATUS ${LLVM_DEFINITIONS})
# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(LLVM_LIBS core support demangle)
message(STATUS ${LLVM_LIBS})

# Create the main program
include_directories(
  inc
  src)
set(BRTO_INC_DIR ${CMAKE_SOURCE_DIR}/inc)
set(BRTO_SRC_DIR ${CMAKE_SOURCE_DIR}/src)
set(BRTO_HDRS
  ${BRTO_SRC_DIR}/ast.hpp
  ${BRTO_SRC_DIR}/parser.hpp
  ${BRTO_SRC_DIR}/visitor.hpp
  ${BRTO_INC_DIR}/driver.hpp)
set(BRTO_SRCS
  ${BRTO_SRC_DIR}/parser.cpp
  ${BRTO_SRC_DIR}/driver.cpp
  ${BRTO_SRC_DIR}/visitor.cpp
  ${BRTO_SRC_DIR}/main.cpp)
add_executable(brto ${BRTO_HDRS} ${BRTO_SRCS})
target_link_libraries(brto
  ${LLVM_LIBS})
