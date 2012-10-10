cmake_minimum_required(VERSION 2.4)

#set(CMAKE_VERBOSE_MAKEFILE 1)

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Ofast -march=native -Wall -std=c++0x")

include_directories(.)
include_directories(../utils)
include_directories(../../kernel)
include_directories(../../user/C++)

add_executable(pfq-n-counters pfq-n-counters.cpp)
add_executable(pfq-histo pfq-histo.cpp)

target_link_libraries(pfq-n-counters -pthread)
