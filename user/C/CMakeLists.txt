cmake_minimum_required(VERSION 2.4)

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O0 -g -march=native -Wall -Wextra -std=c++0x")

include_directories(. ../c++)
link_directories(.)

add_library(pfq libpfq.cpp)

add_executable(test-dispatch test-dispatch.c)
             
target_link_libraries(test-dispatch -lpfq -lstdc++)
