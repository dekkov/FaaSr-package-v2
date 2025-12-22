## R CMD check results

0 errors | 0 warnings | 0 notes

## Submission Notes

This is a new submission to CRAN.

FaaSrLocal provides a local execution environment for FaaSr (Function-as-a-Service in R) workflows. It allows developers to test and debug their FaaSr-compatible workflows on their local machine without requiring cloud infrastructure or credentials.

### Key Features
- Local execution of FaaSr workflow JSON configurations
- FaaSr-compatible API for file operations, logging, and rank management
- Workflow validation including cycle detection and schema validation
- Support for conditional branching and parallel rank execution

### Dependencies
- **Imports**: jsonlite, cli
- **Suggests**: jsonvalidate (for optional schema validation), uuid (for generating invocation IDs), testthat

### Test environments
* Local: Windows 10/11, R 4.4.x
* win-builder (devel and release)
* GitHub Actions: ubuntu-latest, windows-latest, macOS-latest

### Notes on optional dependencies
The package gracefully handles missing optional dependencies:
- If `jsonvalidate` is not installed, schema validation is skipped
- If `uuid` is not installed, a timestamp-based ID is generated instead

