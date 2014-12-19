cmake_minimum_required(VERSION 2.8)

include(${CMAKE_CURRENT_SOURCE_DIR}/../common/CMakeLists.txt)

include_directories(.)
include_directories(../../kernel)
include_directories(../C++)
include_directories(../common/lib)

add_executable(pfq-counters pfq-counters.cpp)
add_executable(pfq-histogram pfq-histogram.cpp)
add_executable(pfq-gen pfq-gen.cpp)
add_executable(pfq-lang pfq-lang.cpp)
add_executable(pfq-bridge pfq-bridge.cpp)

target_link_libraries(pfq-counters -pthread)
target_link_libraries(pfq-histogram -pthread)
target_link_libraries(pfq-gen -pthread -lpcap)
target_link_libraries(pfq-lang -pthread)
target_link_libraries(pfq-bridge -pthread)
