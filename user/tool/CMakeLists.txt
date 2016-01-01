cmake_minimum_required(VERSION 2.8)

project(pfq-tool)

include(CheckIncludeFile)
check_include_file(pcap/pcap.h PCAP_HEADER_FOUND)

if(PCAP_HEADER_FOUND)
    add_definitions(-DHAVE_PCAP_H)
endif(PCAP_HEADER_FOUND)

include(${CMAKE_CURRENT_SOURCE_DIR}/../common/CMakeLists.txt)

include_directories(.)
include_directories(../../kernel)
include_directories(../C++)
include_directories(../common/lib)


add_executable(pfq-counters pfq-counters.cpp)
add_executable(pfq-histogram pfq-histogram.cpp)
add_executable(pfq-gen pfq-gen.cpp)
add_executable(pfq-bridge pfq-bridge.cpp)

target_link_libraries(pfq-counters -pthread -lpfq)
target_link_libraries(pfq-histogram -pthread)
target_link_libraries(pfq-bridge -pthread)

if (PCAP_HEADER_FOUND) 
	target_link_libraries(pfq-gen -pthread -lpcap)
else(PCAP_HEADER_FOUND)
	target_link_libraries(pfq-gen -pthread)
endif(PCAP_HEADER_FOUND)

install (TARGETS pfq-counters DESTINATION bin)
install (TARGETS pfq-gen      DESTINATION bin)
install (TARGETS pfq-bridge   DESTINATION bin)

