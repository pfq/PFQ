cmake_minimum_required(VERSION 2.4)

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -march=native -Wall -Wextra -std=c++0x")

include_directories(. ../C++)

add_library(pfq libpfq.cpp)

