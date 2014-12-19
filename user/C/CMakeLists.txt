cmake_minimum_required(VERSION 2.4)

set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O3 -march=native -Wall -Wextra")

include_directories(../../kernel .)

add_definitions(-D_GNU_SOURCE)

add_library(pfq_static STATIC libpfq.c)
add_library(pfq SHARED libpfq.c)

install_targets(/lib pfq)
install_targets(/lib pfq_static)

install_files(/include/pfq pfq.h)
