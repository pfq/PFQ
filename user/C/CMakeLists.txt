cmake_minimum_required(VERSION 2.4)

set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O3 -march=native -Wall -Wextra")

include_directories(../../kernel .)

add_definitions(-D_GNU_SOURCE)

install_files(/include/pfq pfq.h)
install_files(/lib/ libpfq.a)

add_library(pfq libpfq.c)

