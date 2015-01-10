cmake_minimum_required(VERSION 2.8)

include_directories(.)

add_executable(test-string-view test-string-view.c)
add_executable(test-signature test-signature.c pf_q-signature.c)
add_executable(test-signature-ext test-signature-ext.c pf_q-signature.c)

