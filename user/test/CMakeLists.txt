cmake_minimum_required(VERSION 2.8)

project(pfq-test)

include(CheckIncludeFile)
check_include_file(pcap/pcap.h PCAP_HEADER_FOUND)

include(${CMAKE_CURRENT_SOURCE_DIR}/../common/CMakeLists.txt)

include_directories(../../kernel)

include_directories(../C++)
include_directories(../C)
include_directories(../common/lib)

link_directories(../C)

# C++11 tests

add_executable(test-lang-default test-lang-default.cpp)
add_executable(test-lang-experimental test-lang-experimental.cpp)

add_executable(test-lang-functional test-lang-functional.cpp)
add_executable(test-bloom    test-bloom.cpp)

add_executable(test-dump test-dump.cpp)
add_executable(test-vlan test-vlan.cpp)
add_executable(test-for-range test-for-range.cpp)
add_executable(test-bpf test-bpf.cpp)

add_executable(test-read++ test-read++.cpp)
add_executable(test-send++ test-send++.cpp)

add_executable(test-regression++ test-regression++.cpp)

if (PCAP_HEADER_FOUND)
	add_executable(test-regression-capture test-regression-capture.cpp)
	add_executable(test-regression-pcap-rewrite test-regression-pcap-rewrite.cpp)
endif(PCAP_HEADER_FOUND)

# C++14 tests
                                        
if ("${CMAKE_CXX_FLAGS}" MATCHES "std=c\\+\\+1y")
    add_executable(test-lang++ test-lang.cpp)
endif()

# C tests

add_executable(test-read test-read.c)
add_executable(test-lang test-lang.c)
add_executable(test-send test-send.c)
add_executable(test-dispatch test-dispatch.c)
add_executable(test-regression test-regression.c)

target_link_libraries(test-read -lpfq)
target_link_libraries(test-read++ -lpfq)
target_link_libraries(test-bloom -lpfq)
target_link_libraries(test-send -lpfq)
target_link_libraries(test-send++ -lpfq)
target_link_libraries(test-lang -lpfq)
target_link_libraries(test-dispatch -lpfq)
target_link_libraries(test-regression-capture -lpfq)
target_link_libraries(test-lang-functional -lpfq)
target_link_libraries(test-lang-default -lpfq)
target_link_libraries(test-lang-experimental -lpfq)
target_link_libraries(test-for-range -lpfq)
target_link_libraries(test-dump -lpfq)
target_link_libraries(test-bpf -lpfq)
target_link_libraries(test-vlan -lpfq)

target_link_libraries(test-regression -lpfq -pthread)      
target_link_libraries(test-regression++ -lpfq -pthread)

if (PCAP_HEADER_FOUND)
	target_link_libraries(test-regression-capture -pthread -lpcap)
	target_link_libraries(test-regression-pcap-rewrite -lpcap)
endif(PCAP_HEADER_FOUND)
