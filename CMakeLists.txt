cmake_minimum_required (VERSION 3.11.0)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -fPIC -std=c++11 -O3")

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR})
find_package(R MODULE)
include_directories(${R_INCLUDE_DIR})
include_directories(~/R/x86_64-pc-linux-gnu-library/3.6/Rcpp/include)

find_library(LIB_TO_LINK NAMES)
if(LIB_TO_LINK)
	get_filename_component(LIB_TO_LINK_DIR "${LIB_TO_LINK}" PATH)
	get_filename_component(LIB_TO_LINK_INCLUDE "${LIB_TO_LINK}/../.." ABSOLUTE)
	include_directories(${LIB_TO_LINK_INCLUDE})

	add_library(LSODA4R SHARED LSODA4R.cpp)
	target_link_libraries(LSODA4R ${LIB_TO_LINK})
endif()


