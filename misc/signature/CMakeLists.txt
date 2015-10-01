cmake_minimum_required(VERSION 2.8)

include_directories(. ../../kernel/)

add_executable(test-string-view test-string-view.c)
add_executable(test-signature test-signature.c ../../kernel/lang/signature.c)
add_executable(test-signature-ext test-signature-ext.c ../../kernel/lang/signature.c)

