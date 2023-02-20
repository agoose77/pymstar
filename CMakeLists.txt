# BSD 3-Clause License; see https://github.com/scikit-hep/awkward-1.0/blob/main/LICENSE
cmake_minimum_required(VERSION 3.18...3.24)

# Project must be near the top
project(
  ${SKBUILD_PROJECT_NAME}
  LANGUAGES C Fortran
  VERSION ${SKBUILD_PROJECT_VERSION})


find_package(Python 3.7 COMPONENTS Interpreter NumPy Development.Module REQUIRED)

# F2PY headers
execute_process(
  COMMAND "${PYTHON_EXECUTABLE}"
  -c "import numpy.f2py; print(numpy.f2py.get_include())"
  OUTPUT_VARIABLE F2PY_INCLUDE_DIR
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Prepping the module
set(f2py_module_name "_ext")  # Should match `mstar.pyf`
set(f2py_module_c "${f2py_module_name}module.c")
set(fortran_sig_file "${CMAKE_SOURCE_DIR}/signature/mstar.pyf")
set(fortran_src_files
        "${CMAKE_SOURCE_DIR}/src/msemsg.f"
        "${CMAKE_SOURCE_DIR}/src/mserf1.f"
        "${CMAKE_SOURCE_DIR}/src/msid2z.f"
        "${CMAKE_SOURCE_DIR}/src/mspaul.f"
        "${CMAKE_SOURCE_DIR}/src/msrdb1.f"
        "${CMAKE_SOURCE_DIR}/src/msseal.f"
        "${CMAKE_SOURCE_DIR}/src/msspl1.f"
        "${CMAKE_SOURCE_DIR}/src/msspl2.f"
        "${CMAKE_SOURCE_DIR}/src/mstar1.f")

# Target for enforcing dependencies
add_custom_target(gen_module_c
  DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${f2py_module_c}"
)
add_custom_command(
  OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${f2py_module_c}"
  COMMAND ${PYTHON_EXECUTABLE}  -m "numpy.f2py"
                   "${fortran_sig_file}"
                   --lower
  DEPENDS "${fortran_sig_file}" # Fortran source
)

# Compile C module into Python extension module
Python_add_library("${f2py_module_name}" MODULE WITH_SOABI
            "${CMAKE_CURRENT_BINARY_DIR}/${f2py_module_c}"
            "${F2PY_INCLUDE_DIR}/fortranobject.c"
            ${fortran_src_files})

target_include_directories("${f2py_module_name}" PUBLIC ${F2PY_INCLUDE_DIR})
target_link_libraries("${f2py_module_name}" PRIVATE Python::NumPy)
add_dependencies("${f2py_module_name}" gen_module_c)


# Install python bindings
install(
  TARGETS "${f2py_module_name}"
  LIBRARY DESTINATION "${SKBUILD_PROJECT_NAME}"
  RUNTIME DESTINATION "${SKBUILD_PROJECT_NAME}"
  ARCHIVE DESTINATION "${SKBUILD_PROJECT_NAME}")
