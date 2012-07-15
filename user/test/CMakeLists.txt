cmake_minimum_required(VERSION 2.4)

include_directories(../../kernel)
include_directories(../../user/C++)
include_directories(../../user/C)
link_directories(../../user/C)

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O0 -g2 -Wall -D_GLIBCXX_DEBUG -Wextra -std=c++0x")
set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O0 -g2 -Wall -Wextra")

# C++11 tests


add_executable(test-dump test-dump.cpp)
add_executable(test-vlan test-vlan.cpp)
add_executable(test-for-range test-for-range.cpp)
add_executable(test-regression test-regression.cpp)

# C tests

add_executable(test-read test-read.c)
add_executable(test-dispatch test-dispatch.c)
add_executable(test-regression-c test-regression-c.c)

target_link_libraries(test-read -lpfq)
target_link_libraries(test-dispatch -lpfq)

target_link_libraries(test-regression -pthread)
target_link_libraries(test-regression-c -lpfq -pthread)
