cmake_minimum_required(VERSION 2.4)

#set(CMAKE_VERBOSE_MAKEFILE 1)

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Ofast -march=native -Wall -Wextra -std=c++0x")

include_directories(.)
include_directories(../extra)
include_directories(../../kernel)
include_directories(../../user/C++)

add_executable(pfq-counters pfq-counters.cpp)
add_executable(pfq-histogram pfq-histogram.cpp)
add_executable(pfq-gen pfq-gen.cpp)

target_link_libraries(pfq-counters -pthread)
target_link_libraries(pfq-histogram -pthread)
target_link_libraries(pfq-gen -pthread)
