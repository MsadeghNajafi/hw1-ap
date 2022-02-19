cmake_minimum_required(VERSION 3.13)
project(AP)

set(CMAKE_CXX_STANDARD 14)

find_package(GTest REQUIRED)

include_directories(./include)

add_executable(main
        ./main.cpp
        
        ./hw1.cpp
        ./unit_test.cpp
)
target_link_libraries(main
        GTest::GTest
        GTest::Main
)