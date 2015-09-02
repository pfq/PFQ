cmake_minimum_required(VERSION 2.8)

set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS} -Wall -Wextra")

include_directories(../../kernel/)

add_executable(test-maybe test-maybe.c)

