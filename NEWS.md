# FaaSrLocal 1.0.0

This is the initial CRAN release of FaaSrLocal, providing a local execution environment for FaaSr workflows.

## Major Features

* **Workflow Execution Engine** (`faasr_test()`): Execute FaaSr-compatible workflows locally with support for:
  - Sequential and parallel function execution
  - Conditional branching (True/False paths based on function return values)
  - Parallel rank execution with `FunctionName(N)` notation
  - Automatic dependency tracking and execution ordering

* **FaaSr-Compatible API Functions**:
  - `faasr_put_file()`: Upload files to local storage
  - `faasr_get_file()`: Download files from local storage
  - `faasr_delete_file()`: Delete files from local storage
  - `faasr_get_folder_list()`: List files with optional prefix filtering
  - `faasr_log()`: Append timestamped log messages
  - `faasr_rank()`: Get current parallel rank information
  - `faasr_invocation_id()`: Get workflow invocation ID

* **Workflow Validation**:
  - Cycle detection using depth-first search
  - JSON schema validation (when `jsonvalidate` is available)
  - Predecessor consistency checking

## Notes

This package is designed to mirror the FaaSr API for seamless development and testing of workflows before deploying to cloud Function-as-a-Service platforms.

