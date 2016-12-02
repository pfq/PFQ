#
# CMakeLists base config
#
 
# set the project name
#

set(PROJECT_NAME "PFQ")


# gcc compiler-specific options
#
    
execute_process(COMMAND ${CMAKE_CXX_COMPILER} -dumpversion OUTPUT_VARIABLE COMP_VERSION)
    
message(STATUS "${PROJECT_NAME} C   compiler id: ${CMAKE_CXX_COMPILER_ID}")
message(STATUS "${PROJECT_NAME} C++ compiler id: ${CMAKE_CXX_COMPILER_ID}")

if ("${CMAKE_CXX_COMPILER_ID}" MATCHES "GNU")
    if (COMP_VERSION VERSION_LESS 4.8)
        message(FATAL_ERROR "${PROJECT_NAME} requires g++ 4.8 or greater")

    elseif (COMP_VERSION VERSION_EQUAL 4.8)
        set(CXX_FLAGS_COMPAT "-std=c++11")

    elseif (COMP_VERSION VERSION_EQUAL 4.9 OR COMP_VERSION VERSION_GREATER 4.9)
        set(CXX_FLAGS_COMPAT "-std=c++1y -fdiagnostics-color=auto")
    endif ()
elseif ("${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang")

    # clang++ -dumpversion is currently broken...

    set(CXX_FLAGS_COMPAT "-std=c++1y")

else()
    message(FATAL_ERROR "${PROJECT_NAME} requires gcc or clang (CMAKE_CXX_COMPILER_ID=${CMAKE_CXX_COMPILER_ID})")
endif()
        
# C++ compiler-specific options
#

set(CMAKE_CXX_FLAGS                "${CMAKE_CXX_FLAGS} ${CXX_FLAGS_COMPAT} -O3 -march=native -Wall -Wextra")

if (COMP_VERSION VERSION_EQUAL 4.8 OR COMP_VERSION VERSION_GREATER 4.8)
    set(CMAKE_CXX_FLAGS_DEBUG      "${CMAKE_CXX_FLAGS_DEBUG} ${CXX_FLAGS_COMPAT} -Wall -Wextra -Wconversion -Wsign-conversion -fsanitize=address")
else()
    set(CMAKE_CXX_FLAGS_DEBUG      "${CMAKE_CXX_FLAGS_DEBUG} ${CXX_FLAGS_COMPAT} -Wall -Wextra -Wconversion -Wsign-conversion")
endif()

set(CMAKE_CXX_FLAGS_MINSIZEREL     "${CMAKE_CXX_FLAGS_MINSIZEREL} ${CXX_FLAGS_COMPAT} -Wall -Wextra")
set(CMAKE_CXX_FLAGS_RELEASE        "${CMAKE_CXX_FLAGS_RELEASE} ${CXX_FLAGS_COMPAT} -Wall -Wextra")
 

# C compiler-specific options
#

set(CMAKE_C_FLAGS                   "${CMAKE_C_FLAGS} -O3 -march=native -Wall -Wextra")
set(CMAKE_C_FLAGS_DEBUG             "${CMAKE_C_FLAGS_DEBUG} -Wall -Wextra -Wconversion -Wsign-conversion -Wshadow")
set(CMAKE_C_FLAGS_MINSIZEREL        "${CMAKE_C_FLAGS_MINSIZEREL} -Wall -Wextra")
set(CMAKE_C_FLAGS_RELEASE           "${CMAKE_C_FLAGS_RELEASE} -Wall -Wextra")


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
                                                                                         
