# v0.15.1
- Cleanup of exports => internal datatypes are not visible anymore which may be a breaking change for some users

# v0.15.0
- Update to Purescript v0.14.0

# v0.14.1
## General:
- Added CHANGELOG.md file ;-)

# v0.14.0
## CloudWatchLogs:
- The parameter `nextToken` now needs to be specified when calling `describeLogStreams` or `describeLogGroups`. The response now also includes `nextToken`.
