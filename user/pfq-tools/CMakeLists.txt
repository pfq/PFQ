cmake_minimum_required(VERSION 2.8)

project(pfq-tool)

include(CheckIncludeFile)
check_include_file(pcap/pcap.h PCAP_HEADER_FOUND)

if(PCAP_HEADER_FOUND)
    add_definitions(-DHAVE_PCAP_H)
endif(PCAP_HEADER_FOUND)

include(${CMAKE_CURRENT_SOURCE_DIR}/../lib/common/CMakeLists.txt)

include_directories(.)
include_directories(../../kernel)
include_directories(../lib/C++)
include_directories(../lib/common/lib)


add_executable(pfq-gen pfq-gen.cpp)
add_executable(pfq-capture pfq-capture.cpp)
add_executable(pfq-bridge pfq-bridge.cpp)

target_link_libraries(pfq-capture   -pthread -lpfq)
target_link_libraries(pfq-bridge    -pthread -lpfq)

if (PCAP_HEADER_FOUND) 
	target_link_libraries(pfq-gen -pthread -lpcap -lpfq)
else(PCAP_HEADER_FOUND)
	target_link_libraries(pfq-gen -pthread -lpfq)
endif(PCAP_HEADER_FOUND)

install (TARGETS pfq-gen      DESTINATION bin)
install (TARGETS pfq-capture  DESTINATION bin)
install (TARGETS pfq-bridge   DESTINATION bin)

