#
# CMakeLists base config
#
 
# set the project name
#

set(PROJECT_NAME "PFQ")

# set verbose make
#

set(CMAKE_VERBOSE_MAKEFILE "off")


# gcc compiler-specific options
#
    
execute_process(COMMAND ${CMAKE_CXX_COMPILER} -dumpversion OUTPUT_VARIABLE COMP_VERSION)

if ("${CMAKE_CXX_COMPILER_ID}" MATCHES "GNU")
    if (COMP_VERSION VERSION_LESS 4.7)
        message(FATAL_ERROR "${PROJECT_NAME} requires g++ 4.7 or greater")

    elseif (COMP_VERSION VERSION_EQUAL 4.7)
        set(CXX_FLAGS_COMPAT "-std=c++11 -D_GLIBCXX_USE_NANOSLEEP -D_GLIBCXX_USE_SCHED_YIELD")

    elseif (COMP_VERSION VERSION_EQUAL 4.8)
        set(CXX_FLAGS_COMPAT "-std=c++11")

    elseif (COMP_VERSION VERSION_EQUAL 4.9 OR COMP_VERSION VERSION_GREATER 4.9)
        set(CXX_FLAGS_COMPAT "-std=c++1y -fdiagnostics-color=auto")
    endif ()
elseif ("${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang")

    # clang++ -dumpversion is currently broken...

    set(CXX_FLAGS_COMPAT "-std=c++1y")

else()
        message(FATAL_ERROR "${PROJECT_NAME} requires gcc or clang!")
endif()
        
# C++ compiler-specific options
#

set(CMAKE_CXX_FLAGS                "${CXX_FLAGS_COMPAT} -Wall -Wextra -O2")
set(CMAKE_CXX_FLAGS_DEBUG          "${CXX_FLAGS_COMPAT} -Wall -Wextra -O0 -g")
set(CMAKE_CXX_FLAGS_MINSIZEREL     "${CXX_FLAGS_COMPAT} -Wall -Wextra -Os -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELEASE        "${CXX_FLAGS_COMPAT} -Wall -Wextra -O2 -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CXX_FLAGS_COMPAT} -Wall -Wextra -O2 -g")
 

# C compiler-specific options
#

set(CMAKE_C_FLAGS                   "-Wall -Wextra -O2")
set(CMAKE_C_FLAGS_DEBUG             "-Wall -Wextra -O0 -g")
set(CMAKE_C_FLAGS_MINSIZEREL        "-Wall -Wextra -Os -DNDEBUG")
set(CMAKE_C_FLAGS_RELEASE           "-Wall -Wextra -O2 -DNDEBUG")
set(CMAKE_C_FLAGS_RELWITHDEBINFO    "-Wall -Wextra -O2 -g")


string (STRIP ${COMP_VERSION} COMP_STRIP_VERSION)

message(STATUS "[${PROJECT_NAME}] C++ compiler in use: ${CMAKE_CXX_COMPILER}")
message(STATUS "[${PROJECT_NAME}] C++ compiler version: ${COMP_STRIP_VERSION}")
message(STATUS "[${PROJECT_NAME}] build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "[${PROJECT_NAME}] CMAKE_C_FLAGS: \"${CMAKE_C_FLAGS}\"")
message(STATUS "[${PROJECT_NAME}] CMAKE_C_FLAGS_DEBUG: \"${CMAKE_C_FLAGS_DEBUG}\"")
message(STATUS "[${PROJECT_NAME}] CMAKE_C_FLAGS_RELEASE: \"${CMAKE_C_FLAGS_RELEASE}\"")
message(STATUS "[${PROJECT_NAME}] CMAKE_CXX_FLAGS: \"${CMAKE_CXX_FLAGS}\"")
message(STATUS "[${PROJECT_NAME}] CMAKE_CXX_FLAGS_DEBUG: \"${CMAKE_CXX_FLAGS_DEBUG}\"")
message(STATUS "[${PROJECT_NAME}] CMAKE_CXX_FLAGS_RELEASE: \"${CMAKE_CXX_FLAGS_RELEASE}\"")
                                                                                         
