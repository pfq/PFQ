cmake_minimum_required(VERSION 2.4)

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -march=native -Wall -Wextra -std=c++0x")

include_directories(../../kernel)
include_directories(. ../C++)

install_files(/include/pfq pfq.h)
install_files(/lib/ libpfq.a)

add_library(pfq libpfq.cpp)

