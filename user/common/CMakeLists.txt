#
# CMakeLists base config
#
 
# set the project name
#

set(PROJECT_NAME "PFQ")

# set verbose make
#

set(CMAKE_VERBOSE_MAKEFILE "off")

# initialize CXXFLAGS
#

set(CMAKE_CXX_FLAGS                "-Wall -Wextra")
set(CMAKE_CXX_FLAGS_DEBUG          "-O0 -g")
set(CMAKE_CXX_FLAGS_MINSIZEREL     "-Os -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELEASE        "-O4 -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -g")
 
# C compiler-specific options
#

set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O2 -g2 -Wall -Wextra")

# gcc compiler-specific options
#
    
execute_process(COMMAND ${CMAKE_CXX_COMPILER} -dumpversion OUTPUT_VARIABLE COMP_VERSION)

if ("${CMAKE_CXX_COMPILER_ID}" MATCHES "GNU")
    if (COMP_VERSION VERSION_LESS 4.7)
        message(FATAL_ERROR "${PROJECT_NAME} requires g++ 4.7 or greater")

    elseif (COMP_VERSION VERSION_EQUAL 4.7)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -D_GLIBCXX_USE_NANOSLEEP -D_GLIBCXX_USE_SCHED_YIELD")

    elseif (COMP_VERSION VERSION_EQUAL 4.8)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

    elseif (COMP_VERSION VERSION_EQUAL 4.9)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++1y")
    endif ()
elseif ("${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang")

    # clang++ -dumpversion is currently broken...

    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++1y")

else()
        message(FATAL_ERROR "${PROJECT_NAME} requires gcc or clang!")
endif()
        
string (STRIP ${COMP_VERSION} COMP_STRIP_VERSION)

message(STATUS "${PROJECT_NAME} compiler in use: ${CMAKE_CXX_COMPILER}")
message(STATUS "${PROJECT_NAME} compiler version: ${COMP_STRIP_VERSION}")
message(STATUS "${PROJECT_NAME} build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "${PROJECT_NAME} CMAKE_CXX_FLAGS: \"${CMAKE_CXX_FLAGS}\"")
                                                                                         
