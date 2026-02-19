## Submission

This is a new package submission to CRAN.

## Test environments

* local Windows install, R 4.5.2
* local macOS install, R 4.5.2
* GitHub Actions (macOS-latest, windows-latest, ubuntu-latest), R-release
* GitHub Actions (ubuntu-latest), R-devel and R-oldrel-1


## R CMD check results

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

## About This Package

FaaSr is now acompanion package to the FaaSr package (available on CRAN). It provides a local testing environment for FaaSr workflows, allowing developers to validate and test their Function-as-a-Service workflows locally before deploying to cloud platforms.

Key features:
* JSON workflow validation against FaaSr schema
* Simulated S3 storage operations using local filesystem
* Support for conditional branching and parallel execution
* Workflow cycle detection
* No cloud credentials required for testing

The package is intended for development and testing purposes, complementing the main FaaSr package for production cloud deployments.

## Downstream dependencies

There are currently no downstream dependencies for this package.

