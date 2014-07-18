cmake_minimum_required(VERSION 2.8)

include(${CMAKE_CURRENT_SOURCE_DIR}/../common/CMakeLists.txt)

include_directories(.)
include_directories(../common)
include_directories(../../kernel)
include_directories(../../user/C++)

add_executable(pfq-counters pfq-counters.cpp)
add_executable(pfq-histogram pfq-histogram.cpp)
add_executable(pfq-gen pfq-gen.cpp)

target_link_libraries(pfq-counters -pthread)
target_link_libraries(pfq-histogram -pthread)
target_link_libraries(pfq-gen -pthread)
