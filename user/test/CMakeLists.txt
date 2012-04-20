cmake_minimum_required(VERSION 2.4)

include_directories(../../kernel)
include_directories(../../user/C++)
include_directories(../../user/C)
link_directories(../../user/C)

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -march=native -Wall -Wextra -std=c++0x")

# C++11 tests

add_executable(test-regression test-regression.cpp)
add_executable(test-dump test-dump.cpp)
add_executable(test-for-range test-for-range.cpp)

add_executable(test-dispatch test-dispatch.c)
target_link_libraries(test-dispatch -lpfq -lstdc++)
