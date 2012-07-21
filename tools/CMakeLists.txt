cmake_minimum_required(VERSION 2.4)

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -g2 -Wall -Wextra -std=c++0x")

add_executable(pfq-pcap-test-rewrite pfq-pcap-test-rewrite.cpp)

target_link_libraries(pfq-pcap-test-rewrite -lpcap)
