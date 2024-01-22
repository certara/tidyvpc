## Release summary

This is a patch release `1.5.1` that ensures unit tests do not fail when env var `_R_CHECK_DEPENDS_ONLY_=true` in `R CMD check`. As a result, the `cluster` dependency has been moved from `Suggests` to `Imports`.

## Test environments

* Windows 10 Enterprise, R 4.2.1
* Windows Server 2022, R 4.3.1
* macOS 12.6.9, R 4.3.1
* Ubuntu 22.04.3, R 4.3.1

## R CMD check results

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded


