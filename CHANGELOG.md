# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.0.0.14] - 2018-12-25
### Added
- `mergeGraphs`, ability to take not all node properties from DB.
### Changed
- Refactoring.

## [0.0.0.13] - 2018-12-05
### Added
- `ToJSON` and `FromJSON` instances for `Persisted a`.

## [0.0.0.12] - 2018-10-15
### Added
- `REMOVE` query.
### Fixed
- Escaping special characters in text fields.

## [0.0.0.11] - 2018-06-19
### Changed
- Cabal fix.

## [0.0.0.10] - 2018-05-24
### Changed
- Fixed conditions.

## [0.0.0.9] - 2018-05-18
### Added
- New types for conditions in `DSL`.

## [0.0.0.8] - 2018-05-14
### Added
- Added `DSL` for `Cypher`.

## [0.0.0.7] - 2018-04-23
### Added
- Added ability to delete nodes by their `BoltId`s.

## [0.0.0.6] - 2018-04-20
### Added
- Added ability to update properties of the existing node; added ability to choose
if you want to `CREATE` or `MERGE` the relationship.

## [0.0.0.4] - 2018-04-05
### Changed
- More accurate `toNode` on data fields with `Maybe a` type. If the corresponding field in the type is Nothing, this field won't be included to `Node`.

## [0.0.0.3] - 2018-04-05
### Changed
- More accurate `fromNode` on data fields with `Maybe a` type. If there is no such field in Node, data field will be set to Nothing.

## [0.0.0.0] - 2018-02-22
### Added
- Template Haskell code to generate `Node`s and `URelationship`s.
- Simple queries to upload `Node` and `URelationship`.
- Simple query to download `Node`s.
