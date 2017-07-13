cmake_minimum_required(VERSION 2.4)

set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -O3 -march=native -fomit-frame-pointer -Wall -Wextra -Wshadow -Wconversion")

include_directories(../../kernel .)

add_definitions(-D_GNU_SOURCE)

add_library(pfq_static STATIC libpfq.c cJSON.c)
add_library(pfq SHARED libpfq.c cJSON.c)

install_targets(/lib pfq)
install_targets(/lib pfq_static)

install_files(/include/pfq FILES pfq/pfq.h pfq/pfq-int.h)
