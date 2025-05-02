cmake_minimum_required(VERSION 3.10)

project(basic_json)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_CXX_EXTENSIONS OFF)

enable_testing()

add_executable(usecases usecases.cpp)
