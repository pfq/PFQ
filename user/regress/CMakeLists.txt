cmake_minimum_required(VERSION 2.4)

include_directories(../extra)
include_directories(../../kernel)
include_directories(../../user/C++)
include_directories(../../user/C)

link_directories(../../user/C)

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Ofast -g2 -Wall -Wextra -std=c++0x")

# C++11 tests

add_executable(test-regression test-regression.cpp)
add_executable(test-regression-capture test-regression-capture.cpp)
add_executable(pcap-test-rewrite pcap-test-rewrite.cpp)

# C tests

add_executable(test-regression-c test-regression-c.c)

target_link_libraries(test-regression -pthread)
target_link_libraries(test-regression-capture -pthread -lpcap)
target_link_libraries(test-regression-c -lpfq -pthread)      
target_link_libraries(pcap-test-rewrite -lpcap)

